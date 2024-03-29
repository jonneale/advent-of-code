(ns advent-of-code.2022.17
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn print-grid
  [grid]
  (doseq [y (range   (apply max (map second (keys grid))) -1 -1)]
    (doseq [x (range (inc (apply max (map first (keys grid)))))]
      (let [e (grid [x y])
            v (cond (= e 1) "#" (= e 2) "o" :else ".")]
        (print v)))
    (print "\n")))

(def input
    (->> (-> (io/resource "2022/17.txt")
             slurp
             (s/replace #"\n" ""))))

(def empty-rock
  (apply merge
         (for [x (range 3)
               y (range 3)]
           {[x y] 0})))

(defn modify-empty
  [non-empty-cells]
  (reduce #(assoc %1 %2 1) empty-rock non-empty-cells))

(def rock-a
  (modify-empty
   [[0 0]
    [1 0]
    [2 0]
    [3 0]]))

(def rock-b
  (modify-empty
   [[1 0]
    [0 1]
    [1 1]
    [2 1]
    [1 2]]))

(def rock-c
  (modify-empty
   [[2 2]
    [2 1]
    [2 0]
    [1 0]
    [0 0]]))

(def rock-d
  (modify-empty
   [[0 0]
    [0 1]
    [0 2]
    [0 3]]))

(def rock-e
  (modify-empty
   [[0 0]
    [0 1]
    [1 0]
    [1 1]]))

(def all-rocks
  (cycle [rock-a rock-b rock-c rock-d rock-e]))

(def test-input ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")

(def current-mapping
  {\< [-1 0]
   \> [1 0]})

(defn wind-currents
  [input]
  (map current-mapping (cycle input)))

(defn empty-grid
  []
  (apply merge
         (for [x (range 7)
               y (range 1)]
           {[x y] 0})))

(defn update-grid
  [grid wind-move])

(defn get-highest-point
  [grid]
  (let [y-vals (map (comp last first) (filter (fn[[k v]] (= 1 v)) grid))]
    (if (empty? y-vals)
      0
      (inc (apply max y-vals)))))

(defn initial-state
  [input]
  (let [initial-grid (empty-grid)]
    {:grid initial-grid
     :blocks all-rocks
     :number-of-blocks 0
     :block-in-motion nil
     :offset 0
     :wind-currents (wind-currents input)
     :current-wind (first (wind-currents input))
     :highest-point (get-highest-point initial-grid)
     :time 0}))

(defn get-extreme-edge
  [edge block]
  (let [edge-fn (if (= edge :bottom) second first)]
    (apply min
           (map (comp edge-fn first)
                (filter (fn[[k v]]
                          (= 1 v))
                        block)))))

(defn leftmost-point
  [block]
  (get-extreme-edge :left block))

(defn bottom-point
  [block]
  (get-extreme-edge :bottom block))

(defn insert-block
  [new-block highest-point]
  (let [left-edge (leftmost-point new-block)
        bottom-edge (bottom-point new-block)
        x-offset (+ left-edge 2)
        y-offset (+ highest-point bottom-edge 3)]
    (apply merge
           (map (fn[[[x y] v]]
                  {[(+ x x-offset)
                    (+ y y-offset)]
                   v})
                new-block))))

(defn limit-block-x
  [[x _]]
  [(max 0 (min x 7))])

(defn move-is-legal?
  [grid proposed-block-position]
  (empty?
   (remove (fn[[[x y] v]]
             (or (= v 0)
                 (and (>= x 0)
                      (>= y 0)
                      (<= x 6)
                      (not= 1 (grid [x y])))))
           proposed-block-position)))

(defn remove-block-from-grid
  [grid block-old-position]
  (reduce (fn[agg [k v]]
            (if (= v 1)
              (assoc agg k 0)
              agg))
          grid
          block-old-position))

(defn collision-aware-move
  [block-old-position move grid]
  (let [proposed-block-position (apply merge (map (fn[[k v]] {(map + move k) v})
                                                  block-old-position))
        grid-without-block (remove-block-from-grid grid block-old-position)]
    (if (move-is-legal? grid-without-block proposed-block-position)
      proposed-block-position
      block-old-position)))

(defn blocks-with-value
  [x]
  (apply merge (map (partial apply hash-map)
                    (filter (fn[[k v]] (= 1 v)) x))))
(defn settled?
  [new-block-position block-in-motion]
  (let [new-y-values (map last (keys (blocks-with-value new-block-position)))
        old-y-values (map last (keys (blocks-with-value block-in-motion)))]
    (= (sort new-y-values) (sort old-y-values))))


(defn insert-block-into-grid
  [block-position grid]
  (reduce (fn [agg [k v]]
            (if (= v 1)
              (assoc agg k v)
              agg)) grid block-position))

(defn move-block
  [block-old-position block-new-position grid]
  (let [grid-minus-old-block (remove-block-from-grid grid block-old-position)]
    (insert-block-into-grid block-new-position grid-minus-old-block)))

(defn get-new-block-position
  [block-old-position wind grid]
  (let [block-after-wind (collision-aware-move block-old-position wind grid)
        grid-with-wind-move (move-block block-old-position block-after-wind grid)]
    (collision-aware-move block-after-wind [0 -1] grid-with-wind-move)))

(defn pg
  [state]
  (print-grid (:grid state)))


(defn move-block-in-motion
  [{:keys [grid blocks block-in-motion wind-currents time number-of-blocks] :as state}]
  (let [[current-wind & remaining-wind] wind-currents
        new-block-position (get-new-block-position block-in-motion current-wind grid)
        new-grid (move-block block-in-motion new-block-position grid)]
    (merge
     (if (settled? new-block-position block-in-motion)
       {:block-in-motion nil}
       {:block-in-motion new-block-position})
     {:grid new-grid
      :blocks blocks
      :current-wind (first wind-currents)
      :number-of-blocks number-of-blocks
      :wind-currents remaining-wind
      :time (inc time)})))

(defn filled-squares-by-column
  [grid]
  (let [grouped-grid (group-by (comp first first) grid)]
    (map (fn[[k vs]]
           (filter (fn[[coords v]]
                     (= v 1))
                   vs))
         grouped-grid)))

(defn points-by-column
  [grid]
  (let [grouped-grid (group-by (comp first first) grid)]
    (reduce (fn [agg [column vs]]
              (assoc agg column
                     (sort (map (comp last first) vs))))
            {} grouped-grid)))

(defn highest-point-in-column
  [column]
  (->> column
       (map first)
       (sort-by last)
       last))

(defn column-min-maxes
  [highest-points-by-column]
  (reduce (fn [[min-point max-point] [_ column-max]]
            [(min min-point column-max)
             (max max-point column-max)])
          [Double/POSITIVE_INFINITY Double/NEGATIVE_INFINITY] highest-points-by-column))

(defn select-from-low-to-high
  [grid highest-points-by-column]
  (let [[column-mins column-maxes] (column-min-maxes highest-points-by-column)]
    (select-keys grid
                 (for [x (range 8)
                       y (range column-mins (inc column-maxes))]
                   [x y]))))

(defn peak-range
  [grid]
  (let [filled-squares (filled-squares-by-column grid)]
    (if (some empty? filled-squares)
      grid
      (->> filled-squares
           (map highest-point-in-column)
           (select-from-low-to-high grid)))))

(defn reset-state
  [state]
  (->> (state :grid)
       (peak-range)
       (assoc state :grid)))

(defn run-one-step
  [{:keys [block-in-motion] :as starting-state}]
  (if block-in-motion
    (move-block-in-motion starting-state)
    (let [state (reset-state starting-state)
          {:keys [grid blocks wind-currents time number-of-blocks]} state
          [new-block & remaining-blocks] blocks
          highest-point  (get-highest-point grid)
          block-to-insert (insert-block new-block highest-point)]
      {:grid (insert-block-into-grid block-to-insert grid)
       :blocks remaining-blocks
       :number-of-blocks (inc number-of-blocks)
       :current-wind (first wind-currents)
       :block-in-motion block-to-insert
       :wind-currents wind-currents
       :time time})))

(defn single-step
  [state]
  (time (run-one-step state))
  :done)

(defn run-for-n-steps
  [number-of-steps]
  (first
   (drop number-of-steps
         (iterate run-one-step (initial-state test-input)))))

(defn tower-height
  [state]
  (let [newest-block (:block-in-motion state)
        grid-without-newest-block (remove-block-from-grid (:grid state) newest-block)]
    (get-highest-point grid-without-newest-block)))

(defn run-for-n-blocks
  [number-of-blocks]
  (first
   (drop-while #(< (:number-of-blocks %) number-of-blocks)
               (iterate run-one-step (initial-state input)))))

(defn s
  [state]
  (dissoc (dissoc state :blocks) :wind-currents))

(defn solve-part-1
  []
  (tower-height
   (run-for-n-blocks 2023)))

(defn normalise
  [grid]
  (let [[column-min _] (-> grid keys column-min-maxes)]
    (apply merge
           (map (fn[[[x y] v]] {[x (- y column-min)] v}) grid))))

(defn find-peaks
  [state]
  (let [grid (:grid state)]
    (-> (peak-range grid)
        normalise)))

(defn part-2
  []
  (loop [state (initial-state test-input) check? false history []]
    (if (zero? (mod (inc (:time state)) 100000))
      [:fail history]
      (cond check?
            (let [crenelations (find-peaks state)
                  new-state    crenelations #_[crenelations (:current-wind state)]
                  new-history  (if crenelations (conj history new-state) history)]
              (if (contains? history new-state)
                (:time state)
                (recur (run-one-step state) false new-history)))
            (:block-in-motion state)
            (recur (run-one-step state) false history)
            :else
            (recur (run-one-step state) true history)))))
