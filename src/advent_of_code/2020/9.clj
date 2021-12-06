(ns advent-of-code-2020.9
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def input (slurp (io/resource "9.txt")))

(def test-data
  "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576")

(defn parse-input [i]
  (map read-string (s/split i #"\n")))

(defn find-wrong-number
  [i preamble-size]
  (loop [offset 0]
    (let [numbers-to-sum (take preamble-size (drop offset i))
          value (first (drop (+ preamble-size offset) i))]
      (if (some (fn [x]
                  (and (not= (* x 2) value)
                       ((set numbers-to-sum) (- value x))))
                numbers-to-sum)
        (recur (inc offset))
        (nth i (+ preamble-size offset))))))
  

(def answer-part-1 (find-wrong-number (parse-input input) 25))

(defn find-numbers-that-sum-to-wrong-number
  [i number]
  (loop [index 0 offset 0 numbers []]
    (let [new-numbers (conj numbers (nth i (+ index offset)))
          new-total (apply + new-numbers)]
      (cond (and (> (count numbers) 1) (= number new-total))
            new-numbers
            (> new-total number)
            (recur (inc index) 0 [])
            :else
            (recur index (inc offset) new-numbers)))))


(def answer-part-2 (let [result (sort(find-numbers-that-sum-to-wrong-number (parse-input input) answer-part-1))]
                     (+ (first result) (last result))))
