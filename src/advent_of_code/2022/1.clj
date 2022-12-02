(ns advent-of-code.2022.1
  (:require [clojure.java.io :as io]))

(def input
  (->> (-> (io/resource "2022/1.txt")
           slurp
           (clojure.string/split  #"\n\n"))
       (map #(clojure.string/split % #"\n"))
       (map #(map read-string %))))

(defn sum-all-calories
  [i]
  (map #(reduce + %) i))

(defn find-largest-calories
  [i]
  (reduce max
          (sum-all-calories i)))


(defn add-top-three-calories
  [i]
  (reduce + (take 3 (reverse (sort (sum-all-calories i))))))
