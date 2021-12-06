(ns advent-of-code.12
  (:require [clojure.java.io :as io]))

(def input
  "F10
  N3
  F7
  R90
  F11")

(def part-1-input
  (slurp (io/resource "2020/12.txt")))

(defn parse-input
  [i]
  (->> (clojure.string/split i #"\n")
       (map (fn[row] (let [[h & t] (clojure.string/trim row)]
                       [(keyword (str h))
                        (read-string (apply str t))])))))

(defn move
  [state vector]
  (update state :position (partial map + vector)))

(defn rotate
  [state distance]
  (update state :heading (partial + distance)))

(defn move-forward
  [{:keys [heading] :as state} distance]
  (let [heading-radians (* Math/PI (/ heading 180))
        x (* distance (Math/sin heading-radians))
        y (* distance (Math/cos heading-radians))]
    (move state [x y])))

(defn process-command
  [command distance state]
  (case command
    :N
    (move state [0 distance])
    :E
    (move state [distance 0])
    :S
    (move state [0 (- distance)])
    :W
    (move state [(- distance) 0])
    :L
    (rotate state (- distance))
    :R
    (rotate state distance)
    :F
    (move-forward state distance)))

(defn process-input
  [input]
  (reduce (fn[state [command distance]]
            (process-command command distance state))
          {:heading 90
           :position [0 0]}
          (parse-input input)))

(defn round
  [d]
  (let [factor 100]
    (/ (Math/round (* d 100)) 100)))

(defn absolute [x]
  (Math/abs x))

;;;;;;;;;;;;;;;;;;;
;;;;; Part 2

(defn rotate-90
  [[x y]]
  [y (- x)])

(defn rotate-around-origin
  [point degrees]
  (reduce (fn [agg-point _] (rotate-90 agg-point)) point (range (/ degrees 90))))

(defn rotate-waypoint
  [state distance]
  (update state :waypoint #(rotate-around-origin % distance)))

(defn move-towards-waypoint
  [{:keys [waypoint] :as state} distance]
  (let [[waypoint-x waypoint-y] waypoint]
    (move state [(* waypoint-x distance) (* waypoint-y distance)])))

(defn move-waypoint
  [state vector]
  (update state :waypoint (partial map + vector)))

(defn process-command-part-2
  [command distance state]
  (case command
    :N
    (move-waypoint state [0 distance])
    :E
    (move-waypoint state [distance 0])
    :S
    (move-waypoint state [0 (- distance)])
    :W
    (move-waypoint state [(- distance) 0])
    :L
    (rotate-waypoint state (- 360 distance))
    :R
    (rotate-waypoint state distance)
    :F
    (move-towards-waypoint state distance)))

(defn process-input-2
  [input]
  (reduce (fn[state [command distance]]
                (->
                 (process-command-part-2 command distance state)
                 (assoc :command [command distance])))
          {:heading 90
           :position [0 0]
           :waypoint [10 1]}
          (parse-input input)))

(defn calculate-manhatten
  [state]
  (apply + (map absolute (:position state))))

(defn solve-part-1
  []
  (calculate-manhatten (process-input part-1-input)))


(defn solve-part-2
  []
  (calculate-manhatten (process-input-2 part-1-input)))
