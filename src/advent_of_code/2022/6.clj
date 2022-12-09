(ns advent-of-code.2022.6
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))
(def input
    (->> (-> (io/resource "2022/6.txt")
              slurp)))

(defn find-consecutive-different-chars
  [i n v]
  (let [n-consecutive (take n i)]
    (if (= n (count (distinct n-consecutive)))
      (+ n v)
      (recur (rest i) n (inc v)))))
