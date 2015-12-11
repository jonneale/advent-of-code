(ns advent-of-code.7)

;; bn RSHIFT 2 -> bo
;; lf RSHIFT 1 -> ly
;; fo RSHIFT 3 -> fq

;; {[bn bo] (rshift 2)}

(defn- to-unsigned
  [x]
  (if (> 0 x)
    (+ x (* 2 (inc (Short/MAX_VALUE))))
    x))

(def test-input
  ["123 -> x"
   "456 -> y"
   "x AND y -> d"
   "x OR y -> e"
   "x LSHIFT 2 -> f"
   "y RSHIFT 2 -> g"
   "NOT x -> h"
   "NOT y -> i"])

(def input
  (clojure.string/split (slurp "./resources/7.txt") #"\n"))

(defn get-target-node
  [instruction]
  (keyword (last (re-find #"-> (.*)" instruction))))

(defn wire-input
  [circuit first-command instruction]
  (assoc circuit
    (get-target-node instruction)
    (Integer/parseInt first-command)))

(defn wire-not
  [circuit instruction]
  (let [not-node (keyword (last (re-find #"NOT (.*) ->" instruction)))]
    (assoc circuit
      (get-target-node instruction)
      (to-unsigned (bit-not (circuit not-node))))))

(defn parse-connection-input
  [instruction]
  (re-matches #"([^\s]+) ([A-Z]+) ([^\s]+) .*" instruction))

(defmulti wire-gate
  (fn [circuit instruction]
    (let [[_ arg1 command arg2] (parse-connection-input instruction)]
         command)))

(defmethod wire-gate "AND"
  [circuit instruction]
  (let [[_ arg1 _ arg2] (parse-connection-input instruction)
        value-1 (circuit (keyword arg1))
        value-2 (circuit (keyword arg2))]
    (assoc circuit (get-target-node instruction) (to-unsigned (bit-and value-1 value-2)))))

(defmethod wire-gate "OR"
  [circuit instruction]
  (let [[_ arg1 _ arg2] (parse-connection-input instruction)
        value-1 (circuit (keyword arg1))
        value-2 (circuit (keyword arg2))]
    (assoc circuit (get-target-node instruction) (to-unsigned (bit-or value-1 value-2)))))

(defmethod wire-gate "LSHIFT"
  [circuit instruction]
  (let [[_ arg1 _ arg2] (parse-connection-input instruction)
        value-1 (circuit (keyword arg1))
        value-2 (Integer/parseInt arg2)]
    (assoc circuit (get-target-node instruction) (to-unsigned (bit-shift-left value-1 value-2)))))

(defmethod wire-gate "RSHIFT"
  [circuit instruction]
  (let [[_ arg1 _ arg2] (parse-connection-input instruction)
        value-1 (circuit (keyword arg1))
        value-2 (Integer/parseInt arg2)]
    (assoc circuit (get-target-node instruction) (to-unsigned (bit-shift-right value-1 value-2)))))

(defn wire
  [circuit instruction]
  (let [first-command (re-find #"[^\s]+" instruction)]
    (cond (re-matches #"[0-9]+" first-command)
          (wire-input circuit first-command instruction)
          (re-matches #"NOT" first-command)
          (wire-not circuit instruction)
          :else
          (wire-gate circuit instruction))))

(defn wire-circuit
  [instructions]
  (reduce
   wire
   {}
   instructions))
