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
    (reduce (fn[agg [x y]]
              (let [v (get (get i y) x)]
                (if-let [w (cond (contains? #{\. \#} v) nil
                                 (= \< v) [[x y] [-1 0]]
                                 (= \> v) [[x y] [1 0]]
                                 (= \^ v) [[x y] [0 -1]]
                                 :else [[x y] [0 1]])]
                  (conj agg w)
                  agg)))
            []
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

(defn filter-blizzards
  [blizzards [current-x current-y]]
  (filter (fn[[[x y] _]] (or (= current-x x)
                             (= current-y y)))
          blizzards))

(defn legal-positions-with-blizzards
  [positions blizzards]
  (remove #(get blizzards %) positions))

(defn legal-moves
  [{:keys [current-position time-step max-values] :as state} blizzards]
  (let [[max-x max-y]                          max-values
        possible-positions                     (possible-moves state)
        relevant-blizzards                     blizzards #_(filter-blizzards blizzards current-position)
        current-blizzard-positions             (advance-wind-to-timestep relevant-blizzards time-step max-values)
        legal-new-positions                    (legal-positions-with-blizzards possible-positions current-blizzard-positions)]
    legal-new-positions))

(defn advance-one-step
  [state blizzards]
  (let [advanced-state (update state :time-step inc)]
    (for [position (legal-moves advanced-state blizzards)]
      (-> advanced-state
          (assoc :current-position position)))))

(def start-time (atom nil))

(defn solve-part-1
  [i]
  (let [blizzards (:blizzards i)
        state     (dissoc i :blizzards)]
    (reset! start-time (quot (System/currentTimeMillis) 1000))
    (loop [agenda #{state} iteration 0]
      (let [[head & tail] (sort-by :time-step agenda)]
        (when (zero? (mod iteration 10000))
          (let [time-now (quot (System/currentTimeMillis) 1000)]
            (println "Total time elapsed: " (- time-now @start-time)  "s")
            (println iteration)
            (println (:time-step head))
            (println (count agenda))))
        (if (= (:current-position head) (:goal head))
          [(:time-step head)
           (count agenda)
           iteration
           (:path head)]
          (recur (set (concat tail (advance-one-step head blizzards))) (inc iteration)))))))


(defn print-grid
  [{:keys [goal current-position blizzards max-values time-step] :as state} & [override-time-step]]
  (let [time (or override-time-step time-step)]
    (let [[max-x max-y]        max-values
          blizzard-locations   (set (advance-wind-to-timestep blizzards time max-values))]
      (doseq [y (range (inc max-y))]
        (doseq [x (range (inc max-x))]
          (cond (= [x y] current-position)
                (print"!")
                (= [x y] goal)
                (print "E")
                (or (= x 0)
                    (= y 0)
                    (= x max-x)
                    (= y max-y))
                (print "#")
                (contains? blizzard-locations [x y])
                (print "B")
                :else
                (print ".")))
        (print "\n")))))


(defn solve-part-2
  [{:keys [starting-position goal] :as input}]
  (let [reverse-input (-> input
                          (assoc :starting-position goal)
                          (assoc :current-position goal)
                          (assoc :goal starting-position)
                          (assoc :time-step 18))]
    (solve-part-1 reverse-input)))
