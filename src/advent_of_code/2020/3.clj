(ns advent-of-code.2020.3
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def test-input (slurp (io/resource "2020/3-test.txt")))


(def input (slurp (io/resource "2020/3.txt")))

(defn parse-input [i]
  (-> i (s/split #"\n")))



(defn tree?
  [x]
  (= \# x))



(defn count-trees
  [i x-slope]
  (first (reduce (fn [[tree-count current-x] row]
                   (if (and (= (double current-x) (double (int current-x)))
                            (tree? (nth row (mod current-x (count row)))))
                     [(inc tree-count) (+ current-x x-slope)]
                     [tree-count (+ current-x x-slope)]))
                 [0 0] (parse-input i))))



(defn process-slopes
  []
  (reduce (fn [tree-count [x-slope y-slope]] (* tree-count (count-trees input (/ x-slope y-slope))))
          1
          [[1 1]
           [3 1]
           [5 1]
           [7 1]
           [1 2]]))
