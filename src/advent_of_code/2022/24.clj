(ns advent-of-code.2022.24
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :as set]))

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

(defn print-grid
  [{:keys [exit current-position blizzard-locations max-values]}]
  (let [[max-x max-y] max-values]
    (doseq [y (range 0 max-y)]
      (doseq [x (range 0 max-x)]
        (cond (= [x y] current-position)
              (print"!")
              (= [x y] exit)
              (print "E")
              (or (= x 0)
                  (= y 0)
                  (= x (dec max-x))
                  (= y (dec max-y)))
              (print "#")
              (contains? blizzard-locations [x y])
              (print "B")
              :else
              (print ".")))
      (print "\n"))))

(def input
  (-> (io/resource "2022/24.txt")
      slurp
      (clojure.string/split  #"\n")))

(def directions
  {\< [-1 0]
   \> [1 0]
   \^ [0 -1]
   \v [0 1]})

(defn add-starting-position
  [i state]
  (let [starting-x        (.indexOf (first i) ".")
        exit-x            (.indexOf (last i) ".")]
    (-> state
        (assoc :current-position [starting-x 0])
        (assoc :entrance [starting-x 0])
        (assoc :exit [exit-x (dec (count i))]))))

(defn parse-input
  [i]
  (let [max-y (count i)
        max-x (count (first i))
        all-blizzards (for [y (range max-y)
                            x (range max-x)
                            :when (contains? (set (keys directions)) (get (get i y) x))]
                        (let [v (get (get i y) x)]
                          [v [x y]]))]
    (add-starting-position i {:blizzards all-blizzards
                              :blizzard-locations (set (map last all-blizzards))
                              :max-values [max-x max-y]
                              :moves 0
                              :path []})))

(defn out-of-bounds?
  [entrance exit [max-x max-y] [x y :as coords]]
  (and
   (not= entrance coords)
   (or
    (>= 0 y)
    (>= 0 x)
    (>= x max-x)
    (and (>= y max-y) (not= exit coords)))))

(defn move
  [wind-direction [x y] initial-maxes]
  (let [[max-x max-y]  (map dec initial-maxes)
        [dx dy]        (directions wind-direction)
        [new-x new-y]  [(+ x dx) (+ y dy)]]
    (cond (>= new-x max-x)
          [1 new-y]
          (>= new-y max-y)
          [new-x 1]
          (>= 0 new-x)
          [(dec max-x) new-y]
          (>= 0 new-y)
          [new-x (dec max-y)]
          :else
          [new-x new-y])))

(defn update-blizzards
  [{:keys [blizzards max-values] :as state}]
  (reduce
   (fn[[updated-blizzards list-of-blizzard-locations] [blizzard-direction blizzard-location]]
     (let [new-blizzard-location (move blizzard-direction blizzard-location max-values)]
       [(conj updated-blizzards [blizzard-direction new-blizzard-location])
        (conj list-of-blizzard-locations new-blizzard-location)]))
   [[] #{}]
   blizzards))

(defn get-new-positions
  [{:keys [current-position exit max-values entrance]} blizzard-positions]
  (let [possible-positions (set (map #(map + current-position %) (vals directions)))]
    (remove empty?
            (remove (partial out-of-bounds? entrance exit max-values) (set/difference (into possible-positions #{current-position})
                                                                             blizzard-positions)))))

(defn advance-one-step
  [{:keys [current-position exit max-values] :as i}]
  (if (= current-position exit)
    i
    (let [[updated-blizzards new-blizzard-positions] (update-blizzards i)]
      (for [new-position (get-new-positions i new-blizzard-positions)]
        (-> i
            (assoc :blizzards updated-blizzards)
            (assoc :blizzard-locations new-blizzard-positions)
            (assoc :current-position new-position)
            (update :moves inc)
            #_(update :path #(conj % current-position)))))))

(defn solve-part-1
  [i]
  (loop [agenda #{i} iteration 0]
    (let [[head & tail] (sort-by :moves agenda)]
      (when (zero? (mod iteration 100))
        (do
          (println iteration)
          (println (:moves head))
          (println (count agenda))))
      (if (= (:current-position head) (:exit head))
        [(:moves head)
         (count agenda)
         iteration
         (:path head)]
        (recur (distinct-by (juxt :current-position :moves) (set (concat tail (advance-one-step head)))) (inc iteration))))))
