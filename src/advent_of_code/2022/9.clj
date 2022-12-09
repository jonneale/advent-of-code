(ns advent-of-code.2022.9
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))
(def input
    (->> (-> (io/resource "2022/9.txt")
              slurp
             (clojure.string/split  #"\n"))
         (map #(clojure.string/split % #" "))
         (map (fn[[a b]] [a (read-string b)]))))

(defmulti move
  (fn [direction position]
    direction))

(defmethod move "U"
  [_ [x y]]
  [x (dec y)])

(defmethod move "D"
  [_ [x y]]
  [x (inc y)])

(defmethod move "L"
  [_ [x y]]
  [(dec x) y])

(defmethod move "R"
  [_ [x y]]
  [(inc x) y])

(defn update-tail-position
  [[tail-x tail-y] [head-x head-y] previous-head-position]
  (let [[delta-x delta-y] [(Math/abs (- tail-x head-x)) (Math/abs (- tail-y head-y))]]
    (if (or (> delta-x 1)
            (> delta-y 1))
      previous-head-position
      [tail-x tail-y])))

(defn move-one-in-direction
  [[head-position tail-position] direction]
  (let [new-head-position (move direction head-position)
        new-tail-position (update-tail-position tail-position new-head-position head-position)]
    [new-head-position
     new-tail-position]))

(defn process-command
  [starting-positions [direction number]]
  (reductions (fn[agg _] (move-one-in-direction agg direction)) starting-positions (range number)))

(defn process-commands
  [i]
  (loop [[next-command & rest] i positions [[0 0] [0 0]] paths []]
    (if next-command
      (let [result (process-command positions next-command)
            latest-positions (last result)]
        (recur rest latest-positions (concat paths result)))
      paths)))


(defn find-tail-positions
  [i]
  (distinct (map second (process-commands i))))
#_(defn process-instructions
  [i]
  (reduce process-command [0 0] i))
