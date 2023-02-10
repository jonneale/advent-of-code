(ns advent-of-code.2022.15
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))
(def input
    (->> (-> (io/resource "2022/15.txt")
              slurp
             (clojure.string/split  #"\n"))))

(def test-input
  ["Sensor at x=2, y=18: closest beacon is at x=-2, y=15"
   "Sensor at x=9, y=16: closest beacon is at x=10, y=16"
   "Sensor at x=13, y=2: closest beacon is at x=15, y=3"
   "Sensor at x=12, y=14: closest beacon is at x=10, y=16"
   "Sensor at x=10, y=20: closest beacon is at x=10, y=16"
   "Sensor at x=14, y=17: closest beacon is at x=10, y=16"
   "Sensor at x=8, y=7: closest beacon is at x=2, y=10"
   "Sensor at x=2, y=0: closest beacon is at x=2, y=10"
   "Sensor at x=0, y=11: closest beacon is at x=2, y=10"
   "Sensor at x=20, y=14: closest beacon is at x=25, y=17"
   "Sensor at x=17, y=20: closest beacon is at x=21, y=22"
   "Sensor at x=16, y=7: closest beacon is at x=15, y=3"
   "Sensor at x=14, y=3: closest beacon is at x=15, y=3"
   "Sensor at x=20, y=1: closest beacon is at x=15, y=3"])


(defn parse-coords
  [s]
  (let [[x y] (s/split s #", y=" )]
    (map read-string [x y])))

(defn parse-row
  [row]
  (let [[_ this-coord-string beacon-coord-string] (s/split row #"x=")
        sensor-coord-string (first (s/split this-coord-string #":"))]
    (map parse-coords [sensor-coord-string beacon-coord-string])))

(defn parse-input
  [i]
  (for [row i]
    (parse-row row)))

(defn distance
  [[ax ay] [bx by]]
  (+
   (Math/abs (- ax bx))
   (Math/abs (- ay by))))


(defn overlaps?
  [[current-min current-max] [new-min new-max]]
  (not
   (or (> current-min new-max)
       (< (inc current-max) new-min))))

(defn increases-range?
  [current-gaps new-gap]
  (reduce (fn [any-overlaps? current-gap]
             (or any-overlaps? (overlaps? current-gap new-gap)))
          false current-gaps))

(defn increase-range-of-single-gap
  [[new-min new-max :as new-gap] [current-min current-max :as current-gap]]
  (if (overlaps? current-gap new-gap)
    [(min current-min new-min)
     (max current-max new-max)]
    current-gap))

(defn increase-range
  [current-gaps new-gap]
  (map (partial increase-range-of-single-gap new-gap) current-gaps))

(defn add-new-gap
  [current-gaps new-gap]
  (if (increases-range? current-gaps new-gap)
    (increase-range current-gaps new-gap)
    (conj current-gaps new-gap)))

(defn combine-illegal-point-ranges
  [point-ranges]
  (reduce add-new-gap [[0 0]] point-ranges))

(defn illegal-points-on-row-for-sensor
  [row sensor-x sensor-y beacon-x beacon-y]
  (let [this-sensor-distance-from-beacon (distance [sensor-x sensor-y] [beacon-x beacon-y])
        y-distance-from-this-point      (Math/abs (- sensor-y row))
        x-coords-which-will-be-closer   (- this-sensor-distance-from-beacon
                                           y-distance-from-this-point)]
    (if (< 0 x-coords-which-will-be-closer)
      (sort [(- sensor-x x-coords-which-will-be-closer)
             (+ sensor-x x-coords-which-will-be-closer)])
      [])))

(defn calculate-all-illegal-points-on-row
  [row input]
  (remove empty?
          (for [[[sensor-x sensor-y] [beacon-x beacon-y]] input]
                  (illegal-points-on-row-for-sensor row sensor-x sensor-y beacon-x beacon-y))))

(defn full-range-of-illegal-positions-on-row
  [row input]
  (mapcat (partial apply range)
          (calculate-all-illegal-points-on-row row input)))

(defn count-illegal-points-on-row
  [row input]
  (count (calculate-all-illegal-points-on-row row input)))

(defn fast-illegals-on-row
  [row input]
  (loop [illegals-on-row (calculate-all-illegal-points-on-row row input)]

    (let [combined (sort (combine-illegal-point-ranges illegals-on-row))]
      (if (= illegals-on-row combined)
        combined
        (recur combined)))))

(defn part-1
  [row input]
  (let [all-illegal-ranges (fast-illegals-on-row row input)]
    (reduce (fn[agg [max min]] (+ agg (- min max))) 0 all-illegal-ranges)))

(defn legal-points
  [[previous-max found-so-far] [next-min next-max]]
  (let [new-values (range (inc previous-max) next-min)
        values-to-store (if (empty? new-values) found-so-far (conj found-so-far new-values))]
    [next-max
     values-to-store]))

(defn any-legal-points-between?
  [min max illegal-ranges]
  (reduce legal-points
          [min []]
          (filter (fn[[range-min range-max]]
                    (and (> max range-min)
                         (< min range-max)))
                  illegal-ranges)))

(defn valid-locations-on-each-row
  [input min max]
  (for [row (range 0 max)]
    (let [all-illegal-ranges (fast-illegals-on-row row input)]
      (any-legal-points-between? min max all-illegal-ranges))))

(defn fast-part-2
  [input min max]
  (first
   (drop-while (comp #(= % 1) count last)
               (for [row (range min max)]
                 [row (fast-illegals-on-row row input)]))))


(defn p-fast-part-2
  [input min max]
  (first
   (drop-while (comp #(= % 1) count last)
               (pmap #(vector % (fast-illegals-on-row % input)) (range min max)))))

(defn part-2
  [input min max]
  (take 1
        (remove (comp empty? last) (valid-locations-on-each-row input min max))))

;;11756174628223
