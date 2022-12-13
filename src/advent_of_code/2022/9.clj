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

(defn gap-between-parts-is-too-large?
  [[tail-x tail-y] [head-x head-y]]
  (or (> (Math/abs (- tail-x head-x)) 1)
      (> (Math/abs (- tail-y head-y)) 1)))

(defn on-diagonal?
  [[tail-x tail-y] [head-x head-y]]
  (and (not= tail-x head-x)
       (not= tail-y head-y)))

(defn update-tail-position
  [[tail-x tail-y :as tail] [head-x head-y :as head]]
  (cond
    (not (gap-between-parts-is-too-large? tail head))
    [tail-x tail-y]
    (on-diagonal? tail head)
    [(if (< tail-x head-x)
       (inc tail-x)
       (dec tail-x))
     (if (< tail-y head-y)
       (inc tail-y)
       (dec tail-y))]
    (> (- tail-x head-x) 1)
    (move "L" tail)
    (> (- head-x tail-x) 1)
    (move "R" tail)
    (> (- head-y tail-y) 1)
    (move "D" tail)
    (> (- tail-y head-y) 1)
    (move "U" tail)))

(defn update-positions
  [direction previous-bridge-piece-positions current-bridge-piece-position]
  (if (nil? previous-bridge-piece-positions)
    [(move direction current-bridge-piece-position)]
    (let [previous-piece-after-move         (last previous-bridge-piece-positions)
          current-piece-new-position        (update-tail-position current-bridge-piece-position previous-piece-after-move)]
      (conj previous-bridge-piece-positions current-piece-new-position))))

(defn move-one-in-direction
  [positions direction]
  (reduce (partial update-positions direction) nil positions))

(defn process-command
  [starting-positions [direction number]]
  (reductions (fn[agg _] (move-one-in-direction agg direction)) starting-positions (range number)))

(defn process-commands
  [i starting-positions]
  (loop [[next-command & rest] i positions starting-positions paths []]
    (if next-command
      (let [result (process-command positions next-command)
            latest-positions (last result)]
        (recur rest latest-positions (concat paths result)))
      paths)))


(defn bridge-part-names
  [bridge-parts]
  (apply merge
         (map-indexed (fn[i x]
                        (if (= i 0)
                          {x "H"}
                          {x i}))
                      bridge-parts)))

(defn print-bridge
  [bridge-parts]
  (let [bps (bridge-part-names bridge-parts)]
    (doseq [y (range -20 30)]
      (print "\n")
      (doseq [x (range -20 30)]
        (print (get bps [x y] "."))))))

(defn find-tail-positions
  [i]
  (distinct (map second (process-commands i [[0 0] [0 0]]))))

(defn find-tail-positions-2
  [i]
  (distinct (map last (process-commands i (repeat 10 [0 0])))))
