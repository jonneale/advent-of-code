(ns advent-of-code.2019.5
  (:require [clojure.string :as s]
            [clojure.java.io :as io]))

(def input "3,0,4,0,99")
(def i2 "1002,4,3,4,33")
(def i3 "1101,100,-1,4,0")
(def full-input (slurp (io/resource "2019/5.txt")))

(def i4 "3,9,8,9,10,9,4,9,99,-1,8")
(def i5 "3,9,7,9,10,9,4,9,99,-1,8")

(def i6 "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9")

(def i7 "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99")
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

(defn quick-maffs
  [operator opcode command i position]
  (let [param-modes (drop (count opcode) (reverse (format "%04d" command)))
        params      (get-params i position param-modes)
        value       (apply operator params)
        output-position (i (+ position 3))
        next-position   (+ position 4)]
    [(assoc i output-position value) next-position]))

(defn lt
  [x y]
  (if (< x y) 1 0))

(defn eq
  [x y]
  (if (= x y) 1 0))

(defmulti process-opcode
  (fn [opcode & _]
    opcode))

(defmethod process-opcode "01"
  [opcode command i position]
  (quick-maffs + opcode command i position))

(defmethod process-opcode "02"
  [opcode command i position]
  (quick-maffs * opcode command i position))


(defmethod process-opcode "03"
  [opcode command i position]
  (let [output-location (i (inc position))]
    [(assoc i output-location (:input i)) (+ position 2)]))

(defmethod process-opcode "04"
  [opcode command i position]
  (let [param-modes                   (drop (count opcode)  (reverse (format "%03d" command)))
        [output-value]                (get-params i position param-modes)]
    (println output-value)
    [i (+ position 2)]))

(defn jump-if
  [predicate opcode command i position]
  (let [param-modes                   (drop (count opcode)  (reverse (format "%04d" command)))
        [value-to-test jump-location] (get-params i position param-modes)
        next-position                 (if (predicate value-to-test) jump-location (+ position 3))]
    [i next-position]))

(defmethod process-opcode "05"
  [opcode command i position]
  (jump-if (comp not zero?) opcode command i position))

(defmethod process-opcode "06"
  [opcode command i position]
  (jump-if zero? opcode command i position))

(defmethod process-opcode "07"
  [opcode command i position]
  (quick-maffs lt opcode command i position))

(defmethod process-opcode "08"
  [opcode command i position]
  (quick-maffs eq opcode command i position))

(defmethod process-opcode :default
  [_ _ i & i]
  [i halt])

(defn process-command
  [i position]
  (let [command     (i position)
        str-command (format "%02d" command)
        opcode      (subs str-command (- (count str-command) 2) (count str-command))]
    (process-opcode opcode command i position)))

(defn process-input
  ([i]
   (process-input i 0))
  ([i position]
   (let [[output new-position] (process-command i position)]
     (if (= new-position halt)
       nil
       (recur output new-position)))))


(defn run-with-input-value
  [i]
  (process-input (assoc (tokenize-input full-input) :input i)))
