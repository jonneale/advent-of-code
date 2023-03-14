(ns advent-of-code.2022.24-2
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :as set]
            [clojure.test :refer :all]))

(defn distinct-by
  "Returns a stateful transducer that removes elements by calling f on each step as a uniqueness key.
   Returns a lazy sequence when provided with a collection."
  ([f]
   (fn [rf]
     (let [seen (volatile! #{})]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (let [v (f input)]
            (if (contains? @seen v)
              result
              (do (vswap! seen conj v)
                  (rf result input)))))))))
  ([f xs]
   (sequence (distinct-by f) xs)))

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
#v....#
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
    {:horizontal-blizzards hor
     :vertical-blizzards ver
     :starting-position [1 0]
     :current-position  [1 0]
     :time-step         0
     :max-values        [(dec (count (first i))) (dec (count i))]
     :goal              [(find-goal i) (dec (count i))]
     :path              []}))

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

(defn wrap
  [v wind-direction max-value]
  (if (pos? wind-direction)
    (cond (= v max-value)
          1
          (= 0 (mod v (dec max-value)))
          (dec  max-value)
          (>= v max-value)
          (mod v (dec max-value))
          :else
          v)
    (cond
      (= 0 v)
      (dec max-value)
      (= 0 (mod (Math/abs v) (dec max-value)))
      (dec max-value)
      (<= v 0)
      (let [abs-v (mod (Math/abs v) (dec max-value))]
        (- (dec max-value) abs-v))
      :else
      v)))

(defn move-wind
  [blizzards possible-move-planes time-step max-value]
  (mapcat
   (fn[v]
     (pmap (fn[[offset value]]
             [v (wrap (+ offset (* (inc time-step) value)) value max-value)])
           (blizzards v)))
   possible-move-planes))

(defn move-wind-in-place
  [blizzards possible-move-planes time-step max-value]
  (group-by first
            (mapcat
             (fn[v]
               (pmap (fn[[offset value]]
                       [(wrap (+ offset (* (inc time-step) value)) value max-value) value])
                     (blizzards v)))
             possible-move-planes)))

(defn legal-moves
  [{:keys [current-position horizontal-blizzards vertical-blizzards time-step max-values] :as state}]
  (let [[max-x max-y]                          max-values
        possible-positions                     (possible-moves state)
        relevant-horizontal-blizzard-positions (map reverse (move-wind horizontal-blizzards (map last possible-positions) time-step max-x))
        relevant-vertical-blizzard-positions   (move-wind vertical-blizzards (map first possible-positions) time-step max-y)
        legal-new-positions                    (set/difference  (set possible-positions) (set relevant-horizontal-blizzard-positions) (set relevant-vertical-blizzard-positions))]
    legal-new-positions))

(def start-time (atom nil))

(defn advance-one-step
  [state]
  (for [position (legal-moves state)]
    (-> state
        (assoc :current-position position)
        (update :time-step inc)
        (update :path #(conj % position)))))

(defn solve-part-1
  [i]
  (reset! start-time (quot (System/currentTimeMillis) 1000))
  (loop [agenda #{i} iteration 0]
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
        (recur (distinct-by (juxt :current-position :time-step) (set (concat tail (advance-one-step head)))) (inc iteration))))))

(defn get-wind-final-position-at-step
  [{:keys [max-values vertical-blizzards horizontal-blizzards] :as state} time-step]
  (let [[max-x max-y] max-values
        updated-vertical-blizzards   (move-wind vertical-blizzards (for [x (range max-x)] x) (dec time-step) max-y)
        updated-horizontal-blizzards (map reverse (move-wind horizontal-blizzards (for [y (range max-y)] y) (dec time-step) max-x))]
    [updated-vertical-blizzards updated-horizontal-blizzards]))

(defn move-wind-to-step
  [{:keys [max-values vertical-blizzards horizontal-blizzards] :as state} time-step]
  (let [[max-x max-y] max-values
        updated-vertical-blizzards   (move-wind-in-place vertical-blizzards (for [x (range max-x)] x) (dec time-step) max-y)
        updated-horizontal-blizzards (move-wind-in-place horizontal-blizzards (for [y (range max-y)] y) (dec time-step) max-x)]
    (-> state
        (assoc :vertical-blizzards updated-vertical-blizzards)
        (assoc :horizontal-blizzards updated-horizontal-blizzards))))


(defn print-grid
  [{:keys [goal current-position horizontal-blizzards vertical-blizzards max-values time-step] :as state} & [override-time-step]]
  (let [time (or override-time-step time-step)]
    (let [[max-x max-y]                max-values
          [updated-vertical-blizzards updated-horizontal-blizzards]  (get-wind-final-position-at-step state time)
          blizzard-locations           (set (concat updated-vertical-blizzards updated-horizontal-blizzards))]
      (println "Timestep = " time)
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


(deftest wrap-tests
  ;;wrap v value max
  (is (= 2 (wrap 2 1 6)))
  (is (= 5 (wrap 0 -1 6)))
  (is (= 2 (wrap 2 -1 6)))
  (is (= 3 (wrap -6 -1 4)))
  (is (= 5 (wrap 5 1 6)))
  (is (= 1 (wrap 6 1 6)))
  (is (= 1 (wrap 11 1 6)))
  (is (= 2 (wrap 7 1 6)))
  (is (= 3 (wrap -2 -1 6)))
  (is (= 5 (wrap 10 1 6)))
  (is (= 1 (wrap 1 -1 6)))
  (is (= 5 (wrap 0 -1 6)))
  (is (= 4 (wrap -1 -1 6)))
  (is (= 5 (wrap -5 -1 6)))
  (is (= 7 (wrap -7 -1 8)))
  (is (= 6 (wrap -8 -1 8))))

#_(defn solve-part-2
  [{:keys [starting-position goal] :as s}]
  (let [starting-time-step 245
        starting-state (move-all-wind-to-step s starting-time-step)
        reverse-state  (-> starting-state (assoc :goal starting-position) (assoc :starting-position goal))]
    reverse-state
    #_(solve-part-1 reverse-state)))
