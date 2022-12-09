(ns advent-of-code.2022.8
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def test-input
  ["30373"
   "25512"
   "65332"
   "33549"
   "35390"])

(def input
    (->> (-> (io/resource "2022/8.txt")
              slurp
              (clojure.string/split  #"\n"))))

(defn parse-input
  [i]
  (for [row i]
    (map (comp read-string str) row)))

(defn get-tree-height
  [trees [x y]]
  (nth (nth trees y) x))

(defn neighbour-coords
  [i x y max-x max-y]
  [
   (reverse (for [neighbour-x (range x)]
             [neighbour-x y]))
   (for [neighbour-x (range (inc x) (inc max-x))]
     [neighbour-x y])
   (reverse (for [neighbour-y (range y)]
             [x neighbour-y]))
   (for [neighbour-y (range (inc y) (inc max-y))]
     [x neighbour-y])])

(defn get-neighbours
  [i x y max-x max-y]
  (for [direction (neighbour-coords i x y max-x max-y)]
    (map (partial get-tree-height i) direction)))

(defn tree-visible-in-any-direction?
  [i x y max-x max-y]
  (let [tree-height (get-tree-height i [x y])
        directions  (get-neighbours i x y max-x max-y)]
    (reduce (fn[visible? direction]
              (or visible?
                  (> tree-height (reduce max direction)))) false directions)))

(defn find-visible-trees
  [i]
  (let [max-x (dec (count (first i)))
        max-y (dec (count i))]
    (for [x (range (inc max-x))
          y (range (inc max-y))]
      (or (= x max-x)
          (= y max-y)
          (= x 0)
          (= y 0)
          (tree-visible-in-any-direction? i x y max-x max-y)))))

(defn calculate-scenic-score
  [i x y max-x max-y]
  (let [this-tree-height (get-tree-height i [x y])
        directions (get-neighbours i x y max-x max-y)]
    (reduce *
            (map (fn[direction]
                   (cond (empty? direction)
                         0
                         (> this-tree-height (reduce max direction))
                         (count direction)
                         :else
                         (inc (count (take-while #(> this-tree-height %) direction)))))
                 directions))))


(defn calculate-scenic-scores
  [i]
  (let [max-x (dec (count (first i)))
        max-y (dec (count i))]
    (for [x (range (inc max-x))
          y (range (inc max-y))]
      (calculate-scenic-score i x y max-x max-y))))


(defn calculate-highest-scenic-score
  [i]
  (reduce max (calculate-scenic-scores i)))
