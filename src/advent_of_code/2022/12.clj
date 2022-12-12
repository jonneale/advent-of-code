(ns advent-of-code.2022.12
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def test-input
  "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi")

(def input
    (->> (-> (io/resource "2022/12.txt")
              slurp
              )))

(defn parse-input
  [i]
  (apply merge
         (map-indexed (fn[y row]
                        (apply merge (map-indexed (fn[x value]
                                                    {[x y] value}) row))) (s/split i #"\n"))))

(defn neighbours
  [[x y] grid]
  (let [left   [(dec x) y]
        right  [(inc x) y]
        up     [x (inc y)]
        down   [x (dec y)]]
    (remove nil?
            (map (fn [coords]
                   (let [v (grid coords)]
                     (when v
                       [coords v])))
                 [left right up down]))))

(defn allowed-based-on-height?
  [neighbours this-height]
  (filter (fn [neighbour]
            (or (and (= \a (last neighbour))
                     (= \S this-height))
                (and (= \E (last neighbour))
                     (= \z this-height))
                (and (not= \E (last neighbour))
                     (> 2 (- (int (last neighbour)) (int this-height))))))
          neighbours))

(defn find-start-and-end-positions
  [input]
  (let [start (first (first (filter (fn[[coord value]] (= value \S)) input)))
        end   (first (first (filter (fn[[coord value]] (= value \E)) input)))]
    [start end]))

(defn get-legal-neighbours
  [current-position grid]
  (let [possible-next-steps (neighbours current-position grid)
        next-steps-considering-height (allowed-based-on-height? possible-next-steps (grid current-position))]
    (map first next-steps-considering-height)))

(defn grid->costs
  [grid start]
  (assoc
   (apply merge
          (map (comp (partial apply hash-map) #(vector % Double/POSITIVE_INFINITY)) (keys grid)))
   start
   0))

(defn to-map
  [x]
  (apply hash-map (apply concat x)))


(defn update-unvisited
  [unvisited cost neighbours]
  (reduce (fn[cost-map neighbour]
            (let [new-cost (inc cost)]
              (if (< (cost-map neighbour) new-cost)
                cost-map
                (assoc cost-map neighbour new-cost))))
          (to-map unvisited)
          neighbours))

(defn d-step
  [unvisited visited grid]
  (let [[[first-node cost] & rest] (sort-by last unvisited)
        updated-visited (vec (conj visited [first-node cost]))
        neighbours (get-legal-neighbours first-node grid)
        new-neighbours (clojure.set/difference (set neighbours) (set (map first updated-visited)))]
    [(update-unvisited rest cost new-neighbours) updated-visited]))

(defn dijkstras-algo
  [grid start end]
  (last
   (let [initial-unvisited (grid->costs grid start)]
     (reduce (fn[[unvisited visited] _]
               (d-step unvisited visited grid))
             [initial-unvisited []] initial-unvisited))))

(defn find-path
  [i]
  (let [[start end] (find-start-and-end-positions i)
        result      (dijkstras-algo i start end)]
    ((to-map result) end)))

(defn solve-part-1
  []
  (find-path (parse-input input)))

(defn find-all-starts
  [input]
  (let [start (map first (filter (fn[[coord value]] (= value \a)) input))
        end   (first (first (filter (fn[[coord value]] (= value \E)) input)))]
    [start end]))


(defn solve-part-2
  [input]
  (let [[all-starting-positions end] (find-all-starts input)]
    ()))
