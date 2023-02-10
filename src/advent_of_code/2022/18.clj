(ns advent-of-code.2022.18
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :as set]))
(def input
    (->> (-> (io/resource "2022/18.txt")
              slurp
              (clojure.string/split  #"\n"))
         (map #(map read-string (s/split % #",")))))


(defn calculate-sides
  [[x y z]]
  (for [[x-offset y-offset z-offset] [[-1 0 0] [1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]]]
    [(+ x-offset x)
     (+ y-offset y)
     (+ z-offset z)]))

(def test-data
  [[2,2,2]
   [1,2,2]
   [3,2,2]
   [2,1,2]
   [2,3,2]
   [2,2,1]
   [2,2,3]
   [2,2,4]
   [2,2,6]
   [1,2,5]
   [3,2,5]
   [2,1,5]
   [2,3,5]])

(def test-2
  [[1,1,1]
   [2,1,1]])

(defn calculate-total-surface-area
  [input]
  (reduce (fn[agg sides]
            (+ agg (count (set/difference (set sides) (set input)))))
          0 (map calculate-sides input)))
