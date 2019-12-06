(ns advent-of-code.2019.5
  (:require [clojure.string :as s]
            [clojure.java.io :as io]))

(def input "3,0,4,0,99")
(def i2 "1002,4,3,4,33")
(def i3 "1101,100,-1,4,0")
(def full-input (slurp (io/resource "2019/5.txt")))

(def x "3,225,1,225,6,6,1100,1,238,225,104,0")

(def position-mode \0)
(def immediate-mode \1)
(def halt "halt")

(defn tokenize-input
  [i]
  (apply merge
         (map-indexed (fn [i x] {i (Integer/parseInt x)})
                      (s/split (s/replace i #"\n" "") #","))))

(defn get-params
  [i position param-modes]
  (map-indexed (fn [ix mode]
                 (if (= mode immediate-mode)
                   (i (+ position (inc ix)))
                   (i (i (+ position (inc ix))))))
               param-modes))

(def debug? false)
(defn p
  [& s]
  (when debug?
    (do
      (doseq [v s] (prn v))
      (println "")
      (Thread/sleep 1000))))

(defmulti process-opcode
  (fn [opcode & _]
    opcode))

(defn quick-maffs
  [operator opcode command i position]
  (let [param-modes (drop (count opcode) (reverse (format "%04d" command)))
        params      (get-params i position param-modes)
        value       (apply operator params)
        output-position (i (+ position 3))
        next-position   (+ position 4)]
    (p "PArams: " params)
    [(assoc i output-position value) next-position]))

(defmethod process-opcode "01"
  [opcode command i position]
  (quick-maffs + opcode command i position))

(defmethod process-opcode "02"
  [opcode command i position]
  (quick-maffs * opcode command i position))

(defmethod process-opcode "03"
  [opcode command i position]
  (let [output-location (i (inc position))]
    (p "storing value " (:input i) " at position " (i (inc position)))
    [(assoc i output-location (:input i)) (+ position 2)]))

(defmethod process-opcode "04"
  [opcode command i position]
  (let [output-location (i (inc position))]
    (p "outputting value at " output-location)
    (println (i output-location))
    [i (+ position 2)]))

(defmethod process-opcode :default
  [_ _ i & i]
  [i halt])

(defn process-command
  [i position]
  (let [command     (i position)
        str-command (format "%02d" command)
        opcode      (subs str-command (- (count str-command) 2) (count str-command))]
    (p "Command to process "  command)
    (p "Opcode " opcode)
    (process-opcode opcode command i position)))

(defn process-input
  ([i]
   (process-input i 0))
  ([i position]
   (let [[output new-position] (process-command i position)]
     (p "New position " new-position)
     (if (= new-position halt)
       (p i)
       (recur output new-position)))))


(defn run-with-input-value
  [i]
  (process-input (assoc (tokenize-input full-input) :input i)))
