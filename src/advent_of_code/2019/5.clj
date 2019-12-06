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

(defn store-input
  [input]
  (fn [location]
    input))

(defn return-input
  [input]
  (fn [location]
    (input location)))

(defn get-params
  [i position param-modes]
  (map-indexed (fn [ix mode]
                 (if (= mode immediate-mode)
                   (i (+ position (inc ix)))
                   (i (i (+ position (inc ix))))))
               param-modes))

(defmulti process
  (fn [command & _]
    command))

(defmethod process :store
  [command i position param-modes]
  (let [[location] (get-params i position [immediate-mode])]
    (assoc i location (i :input))))

(defmethod process :return
  [command i position param-modes]
  (let [[location] (get-params i position [immediate-mode])]
    (println (i location))
    i))


(defmethod process :default
  [command i position param-modes]
  (let [params (get-params i position param-modes)
        output (i (+ position (inc (count params))))]
    (assoc i output (apply command params))))

(def debug? false)
(defn p
  [& s]
  (when debug?
    (do (println (apply str s)
                 (Thread/sleep 1000)))))

(defn parse-command
  [i position]
  (let [command (i position)
        str-command (format "%02d" command)
        opcode (subs str-command (- (count str-command) 2) (count str-command))]
    (p "processing command " opcode)
    (cond (= opcode "01")
          [+ (drop (count opcode) (reverse (format "%04d" command))) 2]
          (= opcode "02")
          [* (drop (count opcode) (reverse (format "%04d" command))) 2]
          (= opcode "03")
          [:store (drop (count opcode) (reverse (format "%03d" command))) 0]
          (= opcode "04")
          [:return (drop (count opcode) (reverse (format "%03d" command))) 0]
          (= opcode "99")
          [halt [] 0]
          :else
          [halt [] 0])))

(defn process-input
  ([i]
   (process-input i 0))
  ([i position]
   (let [[command param-modes number-of-params-ignoring-output] (parse-command i position)
         _ (p "-------------------")
         _ (p "command "       command)
         _ (p "position "     position)
         _ (p "param-modes "  param-modes)
         _ (p "param count "  (count param-modes))
         _ (p "new position " (+ position number-of-params-ignoring-output 2))
         _ (p "-------------------")
         ;; + 2 because command itself and output fields
         new-position (+ position number-of-params-ignoring-output 2)]
     (if (= command halt)
       (p i)
       (recur (process command i position param-modes) new-position)))))


(defn run-with-input-value
  [i]
  (process-input (assoc (tokenize-input full-input) :input i)))
