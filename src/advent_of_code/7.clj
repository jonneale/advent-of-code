(ns advent-of-code.7)

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

(defn- get-target-node
  [instruction]
  (keyword (last (re-find #"-> (.*)" instruction))))

(defn- parse-connection-input
  [instruction]
  (re-matches #"([^\s]*) ([A-Z]+) ([^\s]+) .*" instruction))

(defn- get-first-command
  [instruction]
  (re-find #"[^\s]+" instruction))

(defn- to-int
  [x]
  (Integer/parseInt x))

(defn bsr
  [& args]
  (when (> (count args) 2)
    (println args))
  (apply bit-shift-right args))

(defmulti wire-gate
  (fn [instruction first-command [_ arg1 command arg2]]
    (cond (re-matches #"[0-9]+" first-command)
          "INPUT"
          (re-matches #"NOT" first-command)
          "NOT"
          :else
          command)))

(defmethod wire-gate "AND"
  [instruction _ [_ value-1 _ value-2]]
  [(comp to-unsigned bit-and) (keyword value-1) (keyword value-2)])

(defmethod wire-gate "OR"
  [instruction _ [_ value-1 _ value-2]]
  [(comp to-unsigned bit-or) (keyword value-1) (keyword value-2)])

(defmethod wire-gate "LSHIFT"
  [instruction _ [_ value-1 _ value-2]]
  [(comp to-unsigned bit-shift-left) (keyword value-1) (to-int value-2)])

(defmethod wire-gate "RSHIFT"
  [instruction _ [_ value-1 _ value-2]]
  [(comp to-unsigned bsr) (keyword value-1) (to-int value-2)])

(defmethod wire-gate "NOT"
  [instruction _ [_ value-1 _ value-2]]
  (let [not-node (keyword (last (re-find #"NOT (.*) ->" instruction)))]
    [(comp to-unsigned bit-not) not-node]))

(defmethod wire-gate "INPUT"
  [instruction first-command _]
  [identity (to-int first-command)])

(defmethod wire-gate :default
  [instruction first-command _]
  [identity (keyword first-command)])

(defn- to-command
  [instruction]
  (wire-gate instruction
             (get-first-command instruction)
             (parse-connection-input instruction)))

(defn- wire
  [circuit instruction]
  (assoc circuit
    (get-target-node instruction)
    (to-command instruction)))

(defn- wire-circuit
  [instructions]
  (reduce
   wire
   {}
   instructions))

(defn- find-values
  [wired-circuit args]
  (flatten
   (for [arg args]
     (if (keyword? arg)
       (find-values wired-circuit (rest (wired-circuit arg)))
       arg))))

(defn- execute
  [wired-circuit [node [command & args]]]
  (assoc wired-circuit node
         (apply command (find-values wired-circuit args))))


(defn run-network
  [instructions]
  (let [wired-circuit (wire-circuit instructions)]
    (reduce execute wired-circuit wired-circuit)))
