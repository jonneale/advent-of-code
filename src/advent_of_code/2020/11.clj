(ns advent-of-code-11
  (:require [clojure.java.io :as io]))

(def input
  "L.LL.LL.LL
  LLLLLLL.LL
  L.L.L..L..
  LLLL.LL.LL
  L.LL.LL.LL
  L.LLLLL.LL
  ..L.L.....
  LLLLLLLLLL
  L.LLLLLL.L
  L.LLLLL.LL")

(def i1
  ".......#.
...#.....
.#.......
.........
..#L....#
....#....
.........
#........
...#.....")

(def i2
  ".............
.L.L.#.#.#.#.
.............")

(def i3
  ".##.##.
#.#.#.#
##...##
...L...
##...##
#.#.#.#
.##.##.")


(defn to-map
  [parsed-input]
  (apply merge
         (map (partial apply hash-map)
              (reduce concat
                      (map-indexed (fn [row-number row]
                                     (map-indexed (fn [cell-number cell]
                                                    [[row-number cell-number] cell])
                                                  row))
                                   parsed-input)))))

(defn parse-input
  [input]
  (let [split-input (-> input
                        (clojure.string/replace #" " "")
                        (clojure.string/split #"\n"))]
    (-> split-input
        to-map
        (assoc :height (count split-input))
        (assoc :width (count (first split-input))))))

(def floor \.)
(def empty \L)
(def occupied \#)

(defn cell-floor?
  [cell]
  (= cell floor))

(defn cell-empty?
  [cell]
  (= cell empty))

(defn cell-occupied?
  [cell]
  (= cell occupied))

(defn get-cell
  [row-number cell-number input]
  (when (and (>= row-number 0)
             (>= cell-number 0)
             (< row-number (:height input))
             (< cell-number (:width input)))
    (get input [row-number cell-number])))

(defn p
  [state]
  (doseq [y (range (:height state))]
    (println
     (apply str
                   (for [x (range (:width state))]
                     (get-cell y x state))))))

(defn to-number
  [cell]
  cell
  (if (= cell \#) 1
      0))

(defn get-neighbours
  [input row-number cell-number]
  (for [x-offset [-1 0 1]
        y-offset [-1 0 1]
        :when (not (= x-offset y-offset 0))]
    (get-cell (+ row-number y-offset) (+ cell-number x-offset) input)))

(defn one-or-zero
  [cell-range]
  (if (some cell-occupied? cell-range) 1 0))

(defn first-visible-seat-in-direction
  [direction-cells]
  (first
   (drop-while cell-floor?
               direction-cells)))

(defn get-line-of-sight-neighbours
  [{:keys [width height] :as input} row-number cell-number]
  (let [n    (for [y (range (inc row-number) height)] (get-cell y cell-number input))
        e    (for [x (range (inc cell-number) width)] (get-cell row-number x input))
        s    (for [y (range 0 (inc row-number))] (get-cell y cell-number input))
        w    (for [x (range 0 (inc cell-number))] (get-cell row-number x input))
        ne   (for [offset (range 1 (min (- width cell-number)
                                        (- height row-number)))]
               (get-cell (+ row-number offset) (+ cell-number offset) input))
        nw   (for [offset (range 1 (min (inc cell-number)
                                        (- height row-number)))]
               (get-cell (+ row-number offset) (- cell-number offset) input))
        se   (for [offset (range 1 (min (- width cell-number)
                                        (inc row-number)))]
               (get-cell (- row-number offset) (+ cell-number offset) input))
        sw   (for [offset (range 1 (min (inc cell-number)
                                        (inc row-number)))]
               (get-cell (- row-number offset) (- cell-number offset) input))]
    (map first-visible-seat-in-direction [n e s w ne nw se sw])))

(defn out-of-bounds?
  [{:keys [width height]} y x]
  (not(and (>= x 0)
           (>= y 0)
           (<= x width)
           (<= y height))))

(defn next-coord-in-direction
  [direction offset row-number cell-number]
  (case direction
    :n [(+ row-number offset) cell-number]
    :s [(- row-number offset) cell-number]
    :e [row-number (+ cell-number offset)]
    :w [row-number (- cell-number offset)]
    :ne [(+ row-number offset) (+ cell-number offset)]
    :nw [(+ row-number offset) (- cell-number offset)]
    :se [(- row-number offset) (+ cell-number offset)]
    :sw [(- row-number offset) (- cell-number offset)]))


(defn update-remaining-directions
  [radius directions-left input row-number cell-number found-so-far]
  (reduce (fn[[found valid-directions] direction]
            (let [[y x] (next-coord-in-direction direction radius row-number cell-number)]
              (if (out-of-bounds? input y x)
                [found valid-directions]
                (let [found-cell-value (get-cell y x input)]
                  (if (cell-floor? found-cell-value)
                    [found (conj valid-directions direction)]
                    [(conj found found-cell-value) valid-directions])))))
          [found-so-far []] directions-left))

(defn- ifast-line-of-sight
  [radius directions-left input row-number cell-number found-so-far]
  (if (empty? directions-left)
    found-so-far
    (let [[found valid-directions] (update-remaining-directions radius directions-left input row-number cell-number found-so-far)]
      (recur (inc radius) valid-directions input row-number cell-number found))))

(defn fast-line-of-sight-neighbours
  [{:keys [width height] :as input} row-number cell-number]
  (ifast-line-of-sight 1 [:sw :n :e :s :w :ne :se :nw] input row-number cell-number []))

(defn count-occupied-neighbours
  [input row-number cell-number neighbour-fn]
  (->> (neighbour-fn input row-number cell-number)
       (map to-number)
       (reduce +)))

(defn generic-new-cell-state
  [cell row-number cell-number input find-neighbour-fn max-neighbour-count]
  (if (cell-floor? cell)
    cell
    (let [occupied-neighbours (count-occupied-neighbours input row-number cell-number find-neighbour-fn)]
      (cond (and (cell-empty? cell)
                 (= 0 occupied-neighbours))
            occupied
            (and (cell-occupied? cell)
                 (>= occupied-neighbours max-neighbour-count))
            empty
            :else
            cell))))

(defn new-cell-state-part-1
  [cell row-number cell-number input]
  (generic-new-cell-state cell row-number cell-number input get-neighbours 4))

(defn new-cell-state-part-2
  [cell row-number cell-number input]
  (generic-new-cell-state cell row-number cell-number input fast-line-of-sight-neighbours 5))

(defn run-one-step
  [input update-fn]
  (reduce
   (fn [agg [x y]]
     (assoc agg [y x] (update-fn (get-cell y x input) y x input)))
   input
   (for [x (range (:width input))
         y (range (:height input))]
     [x y])))

(defn run-until-stable
  [input update-fn]
  (let [result (run-one-step input update-fn)]
    (if (= result input)
      result
      (recur result update-fn))))

(defn count-stable-seats
  [input update-fn]
  (->>
   (run-until-stable input update-fn)
   (vals)
   (map to-number)
   (reduce + )))

(def part-1-input
  (->> (io/resource "2020/11.txt")
       (slurp)
       (parse-input)))

(defn solve-part-1
  []
  (count-stable-seats part-1-input new-cell-state-part-1))

(defn solve-part-2
  []
  (count-stable-seats part-1-input new-cell-state-part-2))
