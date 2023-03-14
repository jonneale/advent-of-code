(ns advent-of-code.2022.17-2
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def input
    (->> (-> (io/resource "2022/17.txt")
             slurp
             (s/replace #"\n" ""))))

(def rock-a
  [[]
   []
   [0]
   [0]
   [0]
   [0]
   []])

(def rock-b
  [[]
   []
   [1]
   [0 1 2]
   [1]
   []
   []
   ])

(def rock-c
  [[]
   []
   [0]
   [0]
   [0 1 2]
   []
   []
   ])

(def rock-d
  [[]
   []
   [0 1 2 3]
   []
   []
   []
   []
   ])

(def rock-e
  [[]
   []
   [0 1]
   [0 1]
   []
   []
   []
   ])

(def all-rocks
  (cycle [rock-a rock-b rock-c rock-d rock-e]))

(def test-input ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")

(def current-mapping
  {\< -1
   \> 1})

(defn wind-currents
  [input]
  (cycle (map-indexed (fn[i x] [i (current-mapping x)]) input)))

(def empty-grid
  [[0] [0] [0] [0] [0] [0] [0]])

(defn get-min-max
  [grid]
  (reduce (fn[[min-v max-v] column]
            (let [new-max-v (or (last (sort column)) Double/NEGATIVE_INFINITY)
                  new-min-v (or (first  (sort column)) Double/POSITIVE_INFINITY)]
              [(min min-v new-min-v)
               (max max-v new-max-v)]))
          [Double/POSITIVE_INFINITY
           Double/NEGATIVE_INFINITY]
          grid))

(defn get-highest-point
  [grid]
  (last (get-min-max grid)))

(defn initial-state
  [input]
  (let [initial-grid empty-grid]
    {:grid initial-grid
     :blocks all-rocks
     :number-of-blocks 0
     :block-in-motion nil
     :offset 0
     :wind-currents (wind-currents input)
     :current-wind (first (wind-currents input))
     :highest-point (get-highest-point initial-grid)
     :time 0}))

(defn insert-block
  [new-block highest-point]
  (map (fn[row] (map (partial + 4 highest-point) row)) new-block))


(defn move-block
  [block wind]
  (cond (= -1 wind)
        (when (empty? (first block))
          (concat (rest block) [[]]))
        (= 1 wind)
        (when (empty? (last block))
          (concat [[]] (butlast block)))
        :else
        (map #(map dec %) block)))

(defn out-of-bounds?
  [block]
  (not
   (every? #(every? (comp not neg?) %) block)))

(defn collision-aware-move
  [block grid & [wind]]
  (let [proposed-block-position (move-block block wind)]
    (if (and proposed-block-position
             (not (out-of-bounds? proposed-block-position))
             (first
              (reduce (fn[[agg column] v]
                        (let [column-set (set (nth grid column))]
                          [(and agg
                                (every? #(not (contains? column-set %)) v))
                           (inc column)]))
                      [true 0] proposed-block-position)))
      proposed-block-position
      block)))

(defn blocks-with-value
  [x]
  (apply merge (map (partial apply hash-map)
                    (filter (fn[[k v]] (= 1 v)) x))))

(defn get-new-block-position
  [block-old-position grid wind]
  (let [block-after-wind (collision-aware-move block-old-position grid wind)]
    (collision-aware-move block-after-wind grid)))

(defn settled?
  [new-block-position block-in-motion]
  (= (remove empty? new-block-position) (remove empty? block-in-motion)))

(defn move-block-in-motion
  [{:keys [grid blocks block-in-motion wind-currents time number-of-blocks] :as state}]
  (let [[[_ current-wind] & remaining-wind] wind-currents
        new-block-position (get-new-block-position block-in-motion grid current-wind)]
    (merge
     (if (settled? new-block-position block-in-motion)
       {:block-in-motion nil
        :grid (map concat grid new-block-position)
        :settled true}
       {:block-in-motion new-block-position
        :grid grid})
     {:blocks blocks
      :current-wind (first wind-currents)
      :number-of-blocks number-of-blocks
      :wind-currents remaining-wind
      :time (inc time)})))

(defn reset-state
  [{:keys [grid] :as state}]
  (if (some empty? grid)
    state
    (update state :grid #(map (comp (partial take 20) reverse sort) %))))

(defn run-one-step
  [{:keys [block-in-motion] :as starting-state}]
  (if block-in-motion
    (move-block-in-motion starting-state)
    (let [state (reset-state starting-state)
          {:keys [grid blocks wind-currents time number-of-blocks]} state
          [new-block & remaining-blocks] blocks
          highest-point  (get-highest-point grid)
          block-to-insert (insert-block new-block highest-point)]
      {:grid grid
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
         (iterate run-one-step (initial-state input)))))

(defn run-for-n-blocks
  [number-of-blocks & [state]]
  (let [current-state (or state (initial-state input))]
    (first
     (drop-while #(< (:number-of-blocks %) number-of-blocks)
                 (iterate run-one-step current-state)))))

(defn s
  [state]
  (dissoc (dissoc state :blocks) :wind-currents))

(defn solve-part-1
  []
  (get-highest-point (:grid (run-for-n-blocks 2023))))

(defn normalise-grid
  [grid]
  (let [[min-v _] (get-min-max grid)]
    (map (fn[column] (map #(- % min-v ) column)) grid)))

(defn find-repeats
  [input]
  (loop [state (initial-state input) history #{} i 0]
    (if (> i 1000000)
      [:fail history]
      (if (:block-in-motion state)
        (recur (run-one-step state) history (inc i))
        (let [new-state (run-one-step state)
              normalised-grid (-> new-state :grid normalise-grid)
              wind            (first (:current-wind new-state))
              block           (:block-in-motion new-state)]
          (if (contains? (set (map first history)) normalised-grid)
            [(s new-state) history i]
            (recur new-state (conj history [normalised-grid (:number-of-blocks new-state)]) (inc i))))))))

(defn calculate-repeat-and-offset
  [[repeated-value history _]]
  (let [repeated-value-index (inc (count history))
        normalised-repeat    (normalise-grid (:grid repeated-value))
        initial-repeat       (last (first (filter (fn[[grid block-count]] (= grid normalised-repeat)) history)))]
    [(- repeated-value-index initial-repeat)
     initial-repeat]))

(defn i
  [v]
  (+ 135 (* 35 v)))


(def test-starting-height 263)
(def test-repeat-height   53)


(defn base-height
  [n repeat offset]
  (let [state-at-first-repeat   (run-for-n-blocks (+ repeat offset) (initial-state input))
        height-at-first-repeat  (get-highest-point (:grid state-at-first-repeat))
        height-at-second-repeat (get-highest-point (:grid (run-for-n-blocks (+ (* repeat 2) offset) state-at-first-repeat)))]
    (+ height-at-first-repeat
       (* (- height-at-second-repeat
             height-at-first-repeat)
          (dec (bigint
                (Math/floor (/ (- n offset)
                               repeat))))))))


(def part-2-repeat 1710)
(def part-2-offset 322)
(def one-trillion 1000000000000)

(defn i2
  [v]
  (+ 322 (* 1710 v)))

(def test-repeat-coefficient 28571428563)
(def repeat-coefficient 584795321
  #_(dec (first (drop-while #(> one-trillion (i2 %)) (range)))))


(defn incremental-height
  [additional-blocks repeat offset]
  (- (get-highest-point (:grid (run-for-n-blocks (+ offset repeat additional-blocks 1))))
     (get-highest-point (:grid (run-for-n-blocks (+ repeat offset))))))

(defn solve-part-2
  [repeat-coefficient repeat offset]
  (let [block-count (bigint (+ offset (* repeat repeat-coefficient)))]
    (+ (base-height block-count repeat offset)
       (incremental-height (- one-trillion block-count) repeat offset))))

(defn part-2-answer
  []
  (solve-part-2 repeat-coefficient part-2-repeat part-2-offset))

(defn xs
  []
  (first (drop-while (comp not zero? last) (map #(vector % (mod (i2 (- repeat-coefficient %)) (count input))) (range 10000)))))

(defn stacks-match
  [& args]
  (apply =
          (for [v args]
            (normalise-grid (:grid (run-for-n-blocks (i2 v)))))))


(def x-value (normalise-grid (:grid (run-for-n-blocks 1080))))


(defn stack-matcher
  [a]
  (let [x (+ 135 (* 35 27)) ;; 1080
        y (+ 135 (* 35 a))]
    [a
     (and (= (mod x 40)
             (mod y 40))
          (= x-value
             (normalise-grid (:grid (run-for-n-blocks y)))))]))


(defn find-repeat
  [offset repeat count-of-wind]
  (let [v (+ offset (* repeat x) count-of-wind)]
    (filter (fn [[x v r]] (= r 0))
            (map (fn[x] [x v (mod v)]) (range 30)))))


;;repeats every 35 offset by 135, i.e. first match is 170, then 205, 240 etc we only care about values of x where 135x+35/40=0

;;
;; values that pass stack match include:
;; 27 - (35*27)+135 = 40*27
;; 54 - (35*54)+135 = 75*27
;; 1080
;;first repeat is 35 and 170
;; 35x+135
;; wind is 40x
;; a number x-135 divisible by 5,35 and 40 (i.e. 35 and 40)
;; any two normalised stacks will match,
;; of those, every 40th will match on wind if (mod a 40) = (mod b 40)
;; 27
;;(27+8×3,571,428,567)×35+135


;; (= (normalise-grid (:grid (run-for-n-blocks 1080))) (normalise-grid (:grid (run-for-n-blocks (+ 135 (* 35 124))))))
;; => true
;; (get-highest-point (normalise-grid (:grid (run-for-n-blocks 1080))))
;; 160
;; (get-highest-point (:grid (run-for-n-blocks 1080)))
;; => 1641

(defn print-grid
  [grid]
  (doseq [y (reverse (range (inc (get-highest-point grid))))]
    (doseq [x (range 7)]
      (let [column (set (nth grid x))
            e (contains? column y)]
        (print (if e "#" "."))))
    (print "\n")))

(defn calc-highest-points
  []
  (let [v [(i 1) (i 2) (i 3) (i 10) (i 20) (i 21)]]
    (for [blocks v]
      [blocks
       (get-highest-point (:grid (run-for-n-blocks blocks)))])))



(defn print-state
  [state]
  (let [grid (:grid state)
        block-in-motion (:block-in-motion state)]
    (doseq [y (reverse (range (inc (max (get-highest-point grid)
                                        (get-highest-point (or block-in-motion []))))))]
      (doseq [x (range 7)]
        (let [column (set (nth grid x))
              e (contains? column y)
              f (contains? (set (nth block-in-motion x)) y)]
          (print (if f "%" (if e "#" ".")))))
      (print "\n"))))

(defn pg
  [state]
  (print-grid (:grid state)))
