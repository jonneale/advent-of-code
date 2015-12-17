(ns advent-of-code.7)

(defn- to-unsigned
  [x]
  (if (> 0 x)
    (+ x (* 2 (inc (Short/MAX_VALUE))))
    x))

(def simple-input
  (clojure.string/split
   "x AND y -> d
456 -> y
a -> x
123 -> a"
       #"\n"))

(def test-input
  (clojure.string/split
   "123 -> x
456 -> y
x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> i"
       #"\n"))

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

(defn- keyword-or-int
  [value]
  (if (re-find #"[0-9]+" value)
    (Integer/parseInt value)
    (keyword value)))

(defmulti wire-gate
  (fn [instruction first-command [_ arg1 command arg2]]
    (cond (re-matches #"[0-9]+ ->.*" first-command)
          "INPUT"
          (re-matches #"NOT" first-command)
          "NOT"
          :else
          command)))

(defmethod wire-gate "AND"
  [instruction _ [_ value-1 _ value-2]]
  ["AND" (comp to-unsigned bit-and) (keyword-or-int value-1) (keyword-or-int value-2)])

(defmethod wire-gate "OR"
  [instruction _ [_ value-1 _ value-2]]
  ["OR" (comp to-unsigned bit-or) (keyword-or-int value-1) (keyword-or-int value-2)])

(defmethod wire-gate "LSHIFT"
  [instruction _ [_ value-1 _ value-2]]
  ["LSHIFT" (comp to-unsigned bit-shift-left) (keyword-or-int value-1) (to-int value-2)])

(defmethod wire-gate "RSHIFT"
  [instruction _ [_ value-1 _ value-2]]
  ["RSHIFT" (comp to-unsigned bit-shift-right) (keyword-or-int value-1) (to-int value-2)])

(defmethod wire-gate "NOT"
  [instruction _ [_ value-1 _ value-2]]
  (let [not-node (keyword (last (re-find #"NOT (.*) ->" instruction)))]
    ["NOT" (comp to-unsigned bit-not) not-node]))

(defmethod wire-gate "INPUT"
  [instruction first-command _]
  ["INPUT" identity (to-int first-command)])

(defmethod wire-gate :default
  [instruction first-command _]
  ["PASS-THROUGH" identity (keyword-or-int first-command)])

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

(defn update-args
  [circuit args]
  (for [arg args]
    (let [potential-new-value (circuit arg)]
      (if (number? potential-new-value)
        potential-new-value
        arg))))

(defn run
  [original-circuit]
  (loop [circuit original-circuit nodes-to-update (keys original-circuit)]
    (let [node  (first nodes-to-update)
          value (circuit node)]
      (cond (not node)
            circuit

            (not value)
            (recur circuit (concat (rest nodes-to-update) [node]))

            (number? value)
            (recur circuit (rest nodes-to-update))

            :else
            (let [[name fn & args] value]
              (if (every? number? args)
                (recur (assoc circuit node (apply fn args))
                       (rest nodes-to-update))
                (recur (assoc circuit node (into [name fn] (update-args circuit args)))
                       (concat (rest nodes-to-update) [node]))))))))


(defn answer-1
  []
  (:a (run (wire-circuit input))))

(defn answer-2
  []
  (let [wired-circuit (wire-circuit input)
        a-value (:a (run wired-circuit))
        new-circuit (assoc wired-circuit :b a-value)]
    (:a (run new-circuit))))
