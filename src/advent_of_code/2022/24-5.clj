(ns advent-of-code.2022.24-3
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :as set]
            [clojure.test :refer :all]))

(def t
  (s/split
   "#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#" #"\n"))

(def t2
  (s/split
   "#.#####
#.....#
#.....#
#.....#
#...^.#
#.....#
#####.#" #"\n"))

(def all-directions
  (s/split
   "#.#####
#.....#
#.....#
#.....#
#v
#.....#
#####.#" #"\n"))

(def input
  (-> (io/resource "2022/24.txt")
      slurp
      (clojure.string/split  #"\n")))

;;just take starting positions, add or dec based on current turn. mod ftw!

(def directions
  {\< -1
   \> 1
   \^ -1
   \v 1})

(defn parse-wind
  [i]
  (let [max-y (count i)
        max-x (count (first i))]
    (reduce (fn[[horizontal vertical] [x y]]
              (let [v (get (get i y) x)]
                (if-let [w (cond (contains? #{\. \#} v) nil
                                 (= \< v) [[x y] [-1 0]]
                                 (= \> v) [[x y] [1 0]]
                                 (= \^ v) [[x y] [0 -1]]
                                 :else [[x y] [0 1]])]
                  (conj agg w)
                  agg)))
            [[] []]
            (for [x (range max-x)
                  y (range max-y)]
              [x y]))))

(defn find-goal
  [i]
  (.indexOf (last i) "."))

(defn parse-input
  [i]
  {:blizzards         (parse-wind i)
   :starting-position [1 0]
   :current-position  [1 0]
   :time-step         0
   :max-values        [(dec (count (first i))) (dec (count i))]
   :goal              [(find-goal i) (dec (count i))]
   :path              []})

(defn out-of-bounds?
  [{:keys [starting-position goal max-values]} [x y :as coords]]
  (let [[max-x max-y] max-values]
    (and
     (not= starting-position coords)
     (or
      (>= 0 y)
      (>= 0 x)
      (>= x max-x)
      (and (>= y max-y) (not= goal coords))))))

(defn possible-moves
  [{:keys [current-position] :as state}]
  (let [[x y] current-position]
    (remove (partial out-of-bounds? state)
            (map (fn [[x-offset y-offset]]
                   [(+ x x-offset)
                    (+ y y-offset)])
                 [[0 0]
                  [1 0]
                  [0 1]
                  [-1 0]
                  [0 -1]]))))

(defn wrap-direction
  [v max-v]
  (cond
        (= 0 (mod (Math/abs v) (dec max-v)))
        (dec max-v)
        (<= v 0)
        (let [abs-v (mod (Math/abs v) (dec max-v))]
          (- (dec max-v) abs-v))
        (= 0 (mod v (dec max-v)))
        (dec max-v)
        (>= v max-v)
        (mod v (dec max-v))
        :else
        v))

(defn wrap
  [coords max-values]
  (map wrap-direction coords max-values))

(defn move-wind
  [timestep max-values [[blizzard-x blizzard-y] [delta-x delta-y]]]

  (wrap [(+ blizzard-x (* timestep delta-x))
         (+ blizzard-y (* timestep delta-y))]
        max-values))

(defn advance-wind-to-timestep-memo
  [blizzards timestep max-values]
  (into {}
        (for [blizzard blizzards]
          (let [r (move-wind timestep max-values blizzard)]
            [r r]))))

(def advance-wind-to-timestep (memoize advance-wind-to-timestep-memo))
