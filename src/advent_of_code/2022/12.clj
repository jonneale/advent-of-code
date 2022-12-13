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

;; (defn neighbours
;;   [[x y] grid]
;;   (let [left   [(dec x) y]
;;         right  [(inc x) y]
;;         up     [x (inc y)]
;;         down   [x (dec y)]]
;;     (remove nil?
;;             (map (fn [coords]
;;                    (let [v (grid coords)]
;;                      (when v
;;                        [coords v])))
;;                  [left right up down]))))

;; (defn allowed-based-on-height?
;;   [neighbours this-height]
;;   (filter (fn [neighbour]
;;             (or (and (= \a (last neighbour))
;;                      (= \S this-height))
;;                 (and (= \E (last neighbour))
;;                      (= \z this-height))
;;                 (and (not= \E (last neighbour))
;;                      (not= \S (last neighbour))
;;                      (> 2 (- (int (last neighbour)) (int this-height))))))
;;           neighbours))

(defn allowed?
  [neighbour this]
  (or (and (= \a neighbour)
           (= \S this))
      (and (= \E neighbour)
           (= \z this))
      (and (= \E this))
      (and (not= \E neighbour)
           (not= \S neighbour)
           (> 2 (- (int neighbour) (int this))))))

(defn reverse-allowed?
  [neighbour this]
  (or (and (= \z neighbour)
           (= \E this))
      (and (= \a this)
           (= \S neighbour))
      (and (not= \E this)
           (not= \S this)
           (not= \S neighbour)
           (> 2 (- (int this) (int neighbour))))))

(defn fast-neighbour
  [[x y :as this] grid & [reverse?]]
  (let [left   [(dec x) y]
        right  [(inc x) y]
        up     [x (inc y)]
        down   [x (dec y)]
        this-value (grid this)
        allowed-fn? (if reverse? reverse-allowed? allowed?)]
    (remove nil?
            (for [direction [left right up down]]
              (let [v (grid direction)]
                (when (and v
                           (allowed-fn? v this-value))
                  [direction v]))))))

(defn find-start-and-end-positions
  [input]
  (let [start (first (first (filter (fn[[coord value]] (= value \S)) input)))
        end   (first (first (filter (fn[[coord value]] (= value \E)) input)))]
    [start end]))

(defn get-legal-neighbours
  [current-position grid & [reverse]]
  (let [possible-next-steps (fast-neighbour current-position grid reverse)]
    (map first possible-next-steps)))

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
  [unvisited visited grid & [reverse]]
  (let [[[first-node cost] & rest] (sort-by last unvisited)
        updated-visited (vec (conj visited [first-node cost]))
        neighbours (get-legal-neighbours first-node grid reverse)
        new-neighbours (clojure.set/difference (set neighbours) (set (map first updated-visited)))]
    [(update-unvisited rest cost new-neighbours) updated-visited]))

(defn dijkstras-algo
  [grid start & [reverse]]
  (last
   (let [initial-unvisited (grid->costs grid start)]
     (reduce (fn[[unvisited visited] _]
               (d-step unvisited visited grid reverse))
             [initial-unvisited []] initial-unvisited))))

(defn find-path
  [i]
  (let [[start end] (find-start-and-end-positions i)
        result (dijkstras-algo i start)]
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
  (let [[all-starting-positions end] (find-all-starts input)
        solution                     (to-map (dijkstras-algo input end true))]
    (for [start all-starting-positions]
      (solution start))))

(defn performance-test
  []
  (time
   (do
     (time (dijkstras-algo (parse-input input) [0 1] [88 20]))
     (time (dijkstras-algo (parse-input input) [15 2] [88 20]))
     (time (dijkstras-algo (parse-input input) [69 2] [88 20]))
     nil)))
