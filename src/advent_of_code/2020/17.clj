(ns advent-of-code.2020.17
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))


(defn find-current-extreme-values
  [state]
  (let [values (keys state)
        dimension-count (count (first values))]
    (for [i (range dimension-count)]
      (let [dimension-values (map #(nth % i) values)]
        [0 (inc (reduce max dimension-values))]))))

(def i
  ".#.
..#
###")

(def input (slurp (io/resource "2020/17.txt")))

(def active \#)
(def inactive \.)
(def active? (partial = active))
(def inactive? (partial = inactive))

(defn get-cell
  [cell-coords state]
  (or (state cell-coords) inactive))

(defn parse-input
  [i dimensions]
  (let [additional-dimensions (repeat (- dimensions 2) 1)]
    (apply merge
           (map-indexed (fn[row-number row]
                          (apply merge
                                 (map-indexed (fn[column-number cell]
                                                {(concat [column-number row-number] additional-dimensions) cell}) row))) (s/split i #"\n")))))


(defn permutations
  [l1 l2]
  (for [x l1
        y l2]
    [x y]))

(defn multiple-list-permutations
  [l]
  (let [[l1 & other-lists] l]
    (if (= 1 (count other-lists))
      (permutations l1 (first other-lists))
      (map (partial apply cons) (permutations l1 (multiple-list-permutations other-lists))))))

(defn get-neighbours
  [current-cell-coords state offsets]
  (for [offset offsets]
    (get-cell (map + current-cell-coords offset) state)))

(defn increase-upper-bounds
  [values]
  (map (fn[[lower upper]] [lower (+ 2 upper)]) values))

(defn advance-one-tick
  [state]
  (apply merge
         (let [dimension-upper-and-lower-bounds (increase-upper-bounds (find-current-extreme-values state))
               offsets (remove #(every? (partial = 0) %)
                               (multiple-list-permutations (map (fn [_] (range -1 2)) (range (count dimension-upper-and-lower-bounds)))))]
           (for [cell-coords (multiple-list-permutations (map (partial apply range) dimension-upper-and-lower-bounds))]
             (let [value (get-cell cell-coords state)
                   neighbours (get-neighbours cell-coords state offsets)
                   {active-neighbour-count true inactive-neighbour-count false} (frequencies (map active? neighbours))]
               (cond (= 3 active-neighbour-count)
                     {(map inc cell-coords) active}
                     (= 2 active-neighbour-count)
                     {(map inc cell-coords) value}
                     :else
                     {(map inc cell-coords) inactive}))))))

(defn run-times
  [initial-state n]
  (reduce (fn[agg _] (advance-one-tick agg)) initial-state (range n)))

(defn solve-part-1
  []
  (get (frequencies (vals (run-times (parse-input input 3) 6))) active))

(defn solve-part-2
  []
  (get (frequencies (vals (run-times (parse-input input 4) 6))) active))


(defn pretty-print-grid
  [state]
  (let [[[min-width max-width] [min-height max-height] [min-depth max-depth] & rest] (find-current-extreme-values state)]
    (doseq [z (range min-depth (inc max-depth))]
      (println "Z = " z)
      (doseq [y (range min-height (inc max-height))]
        (println
         (doall
          (apply str
                 (for [x (range min-width (inc max-width))]
                   (get state [x y z])))))))))
