(ns advent-of-code.2022.21
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))
(def input
    (->> (-> (io/resource "2022/21.txt")
              slurp
             (clojure.string/split  #"\n"))))

(defn parse-command
  [command]
  (if (re-matches #"[0-9]+" command)
    (read-string command)
    (s/split command #" ")))

(defn parse-input
  [input]
  (apply merge
         (for [row input]
           (let [[monkey-name command] (s/split row #": ")]
             {monkey-name (parse-command command)}))))

(defn perform
  [operator e1 e2]
  (let [op ({"+" + "-" - "*" * "/" /} operator)]
    (if op
      (op e1 e2)
      [e1 e2])))

(defn resolve
  [node input]
  (if (number? node)
    node
    (let [[first-element operator second-element] node]
      (cond (and (number? first-element)
                 (number? second-element))
            (perform operator first-element second-element)
            (number? first-element)
            (perform operator first-element (resolve (get input second-element) input))
            :else
            (perform operator (resolve (get input first-element) input) (resolve (get input second-element) input))))))

(defn part-1
  [input]
  (let [parsed-input (parse-input input)
        root         (get parsed-input "root")]
    (resolve root parsed-input)))

(defn result-for-value
  [input i]
  (let [updated-input (assoc input "humn" i)
        [v1 _ v2]     (get input "root")]
    (resolve [v1 "=" v2] updated-input)))

(defn part-2
  [input]
  (let [i (parse-input input)]
    (loop [max-v 2999999999999 min-v 19999999999990]
      (let [value (+ min-v (bigint (/ (- max-v min-v) 2)))
            [r1 r2] (result-for-value i value)]
        (cond (= r1 r2) value
              (> r2 r1) (recur max-v value)
              :else (recur value min-v))))))
