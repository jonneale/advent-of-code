(ns advent-of-code.2022.14
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def grid-size 505)

(defn print-grid
  [{:keys [grid] :as state} & [start-x end-x start-y end-y & rest]]
  (doseq [y (range (or start-y 0)  (or end-y (inc grid-size)))]
    (doseq [x (range (or start-x 0) (or end-x (inc grid-size)))]
      (let [e (grid [x y])
            v (cond (= e 1) "#" (= e 2) "o" :else ".")]
        (print v)))
    (print "\n")))r

(defn p
  [state]
  (print-grid state 480 520 0 30))

(defn empty-grid
  []
  (apply merge
         (for [x (range (inc grid-size))
               y (range (inc grid-size))]
           {[x y] 0})))

(defn split-route
  [route]
  (s/split route #" -> "))

(defn split-point
  [point]
  (map read-string (s/split point #",")))

(defn update-map
  [rock-map coords]
  (assoc rock-map coords 1))

(defn generate-points
  [[start-x start-y] [end-x end-y]]
  (let [[first-x second-x] (sort [start-x end-x])
        [first-y second-y] (sort [start-y end-y])]
    (for [x (range first-x (inc second-x))
          y (range first-y (inc second-y))]
      [x y])))

(defn insert-stone-wall
  [[rock-map starting-point] end-point]
  (let [start-coord (split-point starting-point)
        end-coord   (split-point end-point)]
    [(reduce update-map
             rock-map
             (generate-points start-coord end-coord))
     end-point]))

(defn add-rock
  [current-map route-through-rock]
  (first
   (let [[first-rock & rest-rock] (split-route route-through-rock)]
          (reduce insert-stone-wall
                  [current-map first-rock]
                  rest-rock))))

(def input
    (->> (-> (io/resource "2022/14.txt")
              slurp
              (clojure.string/split  #"\n"))))

(def test-input
  ["498,4 -> 498,6 -> 496,6"
   "503,4 -> 502,4 -> 502,9 -> 494,9"])

(def sand-starting-position [500 0])

(defn assoc-sand-position
  [state sand-position]
  (-> state
      (assoc :sand-previous-position sand-position)
      (assoc-in [:grid sand-position] 2)))

(defn remove-sand-position
  [state sand-position]
  (assoc-in state [:grid sand-position] 0))

(defn move-sand
  [state from to]
  (-> state
      (assoc-sand-position to)
      (remove-sand-position from)))

(defn increase-sand-count
  [state]
  (if-let [sand-count (:sand-count state)]
    (update state :sand-count inc)
    (assoc state :sand-count 1)))

(defn add-sand-at-starting-position
  [state]
  (-> state
      (assoc-sand-position sand-starting-position)
      (increase-sand-count)))

(defn label-highest-rock
  [{:keys [grid] :as state}]
  (let [highest-rock (reduce (fn[highest [[_ y] _]] (max highest y)) 0 (filter (comp #(= 1 %) last) grid))]
    (assoc state :highest-rock highest-rock)))

(defn parse-input
  [input]
  (->> input
       (reduce add-rock (empty-grid))
       (assoc {:finished false} :grid)
       (label-highest-rock)
       (add-sand-at-starting-position)))

(defn unblocked?
  [x]
  (or (not x) (= 0 x)))

(defn get-new-sand-position
  [[sand-x sand-y] {:keys [grid floor]}]
  (cond (and floor (= sand-y floor))
        [sand-x sand-y]
        (unblocked? (grid [sand-x (inc sand-y)]))
        [sand-x (inc sand-y)]
        (unblocked? (grid [(dec sand-x) (inc sand-y)]))
        [(dec sand-x) (inc sand-y)]
        (unblocked? (grid [(inc sand-x) (inc sand-y)]))
        [(inc sand-x) (inc sand-y)]
        :else
        [sand-x sand-y]))

(defn remove-sand
  [state sand-previous-position]
  (println sand-previous-position)
  (assoc state [:grid sand-previous-position] 1))

(defn advance-one-step
  [{:keys [floor sand-previous-position grid highest-rock] :as state}]
  (let [sand-new-position (get-new-sand-position sand-previous-position state)]
    (cond (= sand-new-position sand-previous-position sand-starting-position)
          (assoc state :finished true)
          (= sand-new-position sand-previous-position)
          (add-sand-at-starting-position state)
          (> (last sand-new-position) (or floor highest-rock))
          (assoc state :finished true)
          :else
          (move-sand state sand-previous-position sand-new-position))))

(defn proceed-until-finished
  [input]
  (:sand-count
   (first
    (drop-while (comp false? :finished) (iterate advance-one-step input)))))

(defn advance-steps
  [input ticks]

  (first (drop ticks (iterate advance-one-step input))))


(defn solve-part-1
  [input]
  (proceed-until-finished input))

(defn input-with-floor
  [input]
  (assoc input :floor (inc (input :highest-rock))))

(defn solve-part-2
  [input]
  (let [with-floor (input-with-floor input)]
    (proceed-until-finished with-floor)))
