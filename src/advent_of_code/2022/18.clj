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
  ;;missing [1,1,1] and [1,1,2]
  [[0,0,0]
   [1,0,0]
   [2,0,0]
   [0,1,0]
   [0,2,0]
   [1,1,0]
   [1,2,0]
   [2,1,0]
   [2,2,0]

   [0,0,1]
   [1,0,1]
   [2,0,1]
   #_[0,1,1]
   [0,2,1]
   [1,2,1]
   [2,1,1]
   [2,2,1]

   [0,0,2]
   [1,0,2]
   [2,0,2]
   [0,1,2]
   [0,2,2]
   [1,2,2]
   [2,1,2]
   [2,2,2]

   [0,0,3]
   [1,0,3]
   [2,0,3]
   [0,1,3]
   [1,1,3]
   [0,2,3]
   [1,2,3]
   [2,1,3]
   [2,2,3]])

(def test-3
  [[1,1,1]
   [2,1,1]
   [3,1,1]
   [4,1,1]
   [5,1,1]
   [6,1,1]
   [1,2,1]
   [2,2,1]
   [3,2,1]
   [4,2,1]
   [5,2,1]
   [6,2,1]
   [1,3,1]
   [2,3,1]
   [3,3,1]
   [4,3,1]
   [5,3,1]
   [6,3,1]
   [1,1,2]
   [2,1,2]
   [3,1,2]
   [4,1,2]
   [5,1,2]
   [6,1,2]
   [1,2,2]
   [6,2,2]
   [1,3,2]
   [2,3,2]
   [3,3,2]
   [4,3,2]
   [5,3,2]
   [6,3,2]
   [1,1,3]
   [2,1,3]
   [3,1,3]
   [4,1,3]
   [5,1,3]
   [6,1,3]
   [1,2,3]
   [2,2,3]
   [3,2,3]
   [4,2,3]
   [5,2,3]
   [6,2,3]
   [1,3,3]
   [2,3,3]
   [3,3,3]
   [4,3,3]
   [5,3,3]
   [6,3,3]])

(defn calculate-total-surface-area
  [input]
  (reduce (fn[agg sides]
            (+ agg (count (set/difference (set sides) (set input)))))
          0 (map calculate-sides input)))

(defn max-values
  [input]
  (reduce (fn[[[min-x min-y min-z] [max-x max-y max-z]] [current-x current-y current-z]]
            [[(min min-x current-x)
              (min min-y current-y)
              (min min-z current-z)]
             [(max max-x current-x)
              (max max-y current-y)
              (max max-z current-z)]])
          [[Double/POSITIVE_INFINITY Double/POSITIVE_INFINITY Double/POSITIVE_INFINITY]
           [Double/NEGATIVE_INFINITY Double/NEGATIVE_INFINITY Double/NEGATIVE_INFINITY]]
          input))

(defn find-missing-cubes
  [input]
  (let [[[min-x min-y min-z] [max-x max-y max-z]] (max-values input)
        set-input (set input)]
    (remove nil?
            (for [x (range min-x (inc max-x))
                  y (range min-x (inc max-y))
                  z (range min-x (inc max-z))]
              (when-not (contains? set-input [x y z])
                [x y z])))))

(defn surrounding-cube-exists?
  [[x y z] [grouped-x grouped-y grouped-z]]
  (let [x-vals (map first (grouped-x [y z]))
        y-vals (map second (grouped-y [x z]))
        z-vals (map last (grouped-z [x y]))]
    (and (some #(> % x) x-vals)
         (some #(< % x) x-vals)
         (some #(> % y) y-vals)
         (some #(< % y) y-vals)
         (some #(> % z) z-vals)
         (some #(< % z) z-vals))))

(defn solid?
  [neighbour input]
  (contains? (set input) neighbour))

(defn get-neighbours
  [[x y z]]

  [[(inc x) y z]
   [(dec x) y z]
   [x (inc y) z]
   [x (dec y) z]
   [x y (inc z)]
   [x y (dec z)]])

(defn find-enclosed-area
  [input grouped-input cube]
  (loop [paths-from-here [cube] already-seen #{}]
    (let [next-moves (remove #(or (contains? already-seen %)
                                  (solid? % input)) (get-neighbours (first paths-from-here)))]
      (cond (empty? next-moves)
            [(concat paths-from-here already-seen) true]
            (some (complement true?) (map #(surrounding-cube-exists? % grouped-input) next-moves))
            [(concat paths-from-here already-seen) false]
            :else
            (recur (doall (concat (rest paths-from-here) next-moves)) (set (conj already-seen (first paths-from-here))))))))

(defn group-input
  [input]
  [(group-by (juxt #(nth % 1) #(nth % 2)) input)
   (group-by (juxt #(nth % 0) #(nth % 2)) input)
   (group-by (juxt #(nth % 0) #(nth % 1)) input)])

(defn find-enclosed-cube
  [input]
  (let [grouped-input (group-input input)]
    (loop [unchecked (find-missing-cubes input) checked #{}]
      (if (empty? unchecked)
            checked
            (let [[head & tail] unchecked
                  [enclosed-area enclosed?] (find-enclosed-area input grouped-input head)
                  remaining-unchecked       (set/difference (set tail) (set enclosed-area))]
              (println "Found " (count enclosed-area) " squares which were " (if enclosed? "enclosed" "not enclosed"))
              (println (count remaining-unchecked) " remain unchecked")
              (if enclosed?
                (recur remaining-unchecked (set (concat checked enclosed-area)))
                (recur remaining-unchecked checked)))))))

(defn part-2
  [input]
  (- ;; (calculate-total-surface-area input)
     4320
     (calculate-total-surface-area (find-enclosed-cube input))))


;;; for every x and y the biggest z
;;;2444 is too low
;;;4044 is incorrect
