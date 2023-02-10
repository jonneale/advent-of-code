(ns advent-of-code.2022.24-2
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :as set]))


(def t
  (s/split
   "#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#" #"\n"))

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

(defn insert
  [m plane position v]
  (if (m plane)
    (update m plane #(conj % [position v]))
    (assoc m plane [[position v]])))

(defn parse-wind
  [i]
  (let [max-y (count i)
        max-x (count (first i))]
    (reduce (fn[[horizontal-blizzards vertical-blizzards :as agg] [x y]]
              (let [v (get (get i y) x)]
                (cond (contains? #{\. \#} v) agg
                      (contains? #{\< \>} v) [(insert horizontal-blizzards y x (directions v)) vertical-blizzards]
                      :else                  [horizontal-blizzards (insert vertical-blizzards x y (directions v))])))
            [{} {}]
            (for [x (range max-x)
                  y (range max-y)]
              [x y]))))

(defn find-goal
  [i]
  (.indexOf (last i) "."))

(defn parse-input
  [i]
  (let [[hor ver] (parse-wind i)]
    {:horizontal-wind hor
     :vertical-wind ver
     :starting-position [1 0]
     :current-position  [1 0]
     :time-step         0
     :max-values        [(count (first i)) (count i)]
     :goal              [(find-goal i) (count i)]}))

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
            (for [x-offset (range -1 2)
                  y-offset (range -1 2)]
              [(+ x x-offset)
               (+ y y-offset)]))))

(defn move-wind
  [blizzards plane time-step]
  (mapcat
   (fn[v]
     (map (fn[offset value]
            [v (+ offset(* time-step value))])
          (blizzards v)))
     plane))

(defn advance-one-step
  [{:keys [current-position horizontal-blizzards vertical-blizzards time-step] :as state}]
  (let [possible-positions                     (possible-moves state)
        relevant-horizontal-blizzard-positions (move-wind horizontal-blizzards (map first possible-positions) time-step)
        relevant-vertical-blizzard-positions   (map reverse (move-wind vertical-blizzards (map last possible-positions) time-step))]))
