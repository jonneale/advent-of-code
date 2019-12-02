(ns advent-of-code.2018.6
  (:require [clojure.java.io :as io]))


(defn is-bounded-2?
  [other-coords [x y]]
  (reduce (fn [agg [other-x other-y]]
            (if
                (or (and (> other-x x)
                         (> other-y y))
                    (and (> other-x x)
                         (> (- y other-y)
                            (- other-x x)))
                    (and (> other-y y)
                         (> (- x other-x)
                            (- other-y y)))
              [other-x other-y]
              agg))
         other-coords))

(defn is-bounded?
  [other-coords coords]
  ;; could be done more efficiently by putting the fn evaluation in the and but.. meh
  (let [less-than-max-combined (< (reduce + coords) (reduce max (map #(reduce + %) other-coords)))
        north (some #(> (first coords) %) (map first other-coords))
        south (some #(< (first coords) %) (map first other-coords))
        east  (some #(< (second coords) %) (map second other-coords))
        west  (some #(> (second coords) %) (map second other-coords))]
    (and north south east west less-than-max-combined)))

(defn parse-int
  [i]
  (Integer/parseInt i))

(def data
  (map #(map parse-int (clojure.string/split % #", "))
       (clojure.string/split (slurp (io/resource "2018/6.txt")) #"\n")))

(def a [1, 1])
(def b [1, 6])
(def c [8, 3])
(def d [3, 4])
(def e [5, 5])
(def f [8, 9])

(defn calc-distance
  [[a-x a-y] [b-x b-y]]
  (+ (Math/abs (- b-x a-x))
     (Math/abs (- b-y a-y))))

(defn belongs-to
  [[x y] coords]
  (first (first (sort-by last (map #(vector % (calc-distance [x y] %)) coords)))))

(defn find-area
  [coords]
  (let [max-x-value (reduce max (map first coords))
        max-y-value (reduce max (map last coords))]
    ;;the outer edge can only belong to unbounded points
    (for [x (range 1 (dec max-x-value))
          y (range 1 (dec max-y-value))]
      (belongs-to [x y] coords))))

(defn belongs-to?
  [[x y] [this-x this-y] other-coords]
  (let [our-distance (calc-distance [x y] [this-x this-y])]
    (nil? (some #(<= (calc-distance [x y] %) our-distance) other-coords))))

(defn calculate-offset-at-radial-distance
  [i]
  (set
   (reduce concat
           (for [x (range (- 0 i) (inc i))]
             [[x (- 0 i)]
              [(- 0 i) x]
              [x i]
              [i x]]))))

(defn count-neighbours
  [i [x y] others]
  (for [[d-x d-y] (calculate-offset-at-radial-distance i)]
    (let [coords-to-check [(+ x d-x) (+ y d-y)]]
      (when (every? true? (map pos? coords-to-check))
        (belongs-to? coords-to-check
                     [x y]
                     others)))))

(defn radially-search-for-child-points
  [[x y] other-coords]
  (loop [i 1 coord-count 0]
    (let [neighbour-points  (count-neighbours i [x y] other-coords)
          count-of-neighbour-children (count (filter true? neighbour-points))]
      (Thread/sleep 100)
      (println "Level " i)
      (println "Found " count-of-neighbour-children " neighbours")
      (println "Coords " [x y])
      (println "Total neighbours: " (+ coord-count count-of-neighbour-children))
      (println "--------------------------------------------------------")
      (if (= 0 count-of-neighbour-children)
        (inc coord-count)
        (recur (inc i) (+ coord-count count-of-neighbour-children))))))

(defn count-closest-points
  [coords]
  (last
   (sort-by last
            (let [bounded-coords (filter (partial is-bounded? coords) coords)]
              (for [bounded-coord bounded-coords]
                [bounded-coord (radially-search-for-child-points
                                bounded-coord
                                (filter #(not= % bounded-coord) coords))])))) )

(def weird-point [280 341])
(def other-coords (remove #(= % [280 341]) data))
(def x (first weird-point))
(def x (last weird-point))


(def c (doall (remove nil? (for [[d-x d-y] (calculate-offset-at-radial-distance 105)]
                             (let [coords-to-check [(+ x d-x) (+ y d-y)]]
                               (when (every? true? (map pos? coords-to-check))
                                 coords-to-check))))))
(def r (map (fn [coord] [coord (belongs-to? coord weird-point other-coords)]) c))


(def matching-coords (filter (comp true? last) r))
