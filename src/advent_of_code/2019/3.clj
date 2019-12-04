(ns advent-of-code.2019.3
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.set :as set]))

(def input (slurp (io/resource "2019/3.txt")))
(def input2
  "R8,U5,L5,D3
U7,R6,D4,L4")

(defn to-instruction-string
  [instructions]
  (string/split instructions #","))

(defn parse-input
  [i]
  (map to-instruction-string (string/split i #"\n")))

(defn to-range
  [[x dx] [y dy]]
  (for [x1 (range x dx)
        y1 (range y dy)]
    [x1 y1]))

(defn to-operation
  [command distance]
  (let [distance-int (Integer/parseInt distance)]
    (cond (= command \U) [[0 distance-int]     (to-range [0 1] [1 (inc distance-int)])]
          (= command \D) [[0 (- distance-int)] (reverse (to-range [0 1] [(- distance-int) 0]))]
          (= command \L) [[(- distance-int) 0] (reverse (to-range [(- distance-int) 0] [0 1]))]
          (= command \R) [[distance-int 0]     (to-range [1 (inc distance-int)] [0 1])])))

(defn track-coords
  [[current-coord all-previous-coords] [command & distance]]
  (let [[final-operation operation] (to-operation command (apply str distance))
        final-coord (map + final-operation current-coord)]
    [final-coord (into all-previous-coords (generate-coords current-coord operation))]))

(defn wire-route
  [instructions]
  (last (reduce track-coords
                [[0,0] [[0,0]]] instructions)))

(defn calculate-coords-for-wires
  [wires]
  (map (comp set wire-route) wires))

(defn calculate-distance
  [[x y]]
  (+ (Math/abs x)
     (Math/abs y)))

(defn find-intersections
  [instructions]
  (->> (calculate-coords-for-wires instructions)
       (apply set/intersection)
       (sort-by calculate-distance)))

(defn find-closest-intersection
  [i]
  ;; second because the origin will be first
  (second (find-intersections (parse-input i))))

(defn to-indexed-map
  [route]
  (apply merge (map-indexed (fn [i position] {position i}) route)))

(defn intersection-steps
  [route-maps intersection]
  (reduce #(+ %1 (get %2 intersection)) 0 route-maps))

(defn find-intersection-locations
  [wire-routes intersections]
  (let [route-maps (map to-indexed-map wire-routes)]
    (map (partial intersection-steps route-maps) intersections)))

(defn find-steps-until-intersections
  [i]
  (let [input (parse-input i)
        wire-routes (map wire-route input)
        intersections (find-intersections input)]
    (second (sort (find-intersection-locations wire-routes intersections)))))
