(ns advent-of-code.2020.13
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :as set]))

(def i
  "939
7,13,x,x,59,x,31,19")

(def i2
  "0
17,x,13,19")

(def i4
  "0
67,7,x,59,61")

(def i5
  "0
1789,37,47,1889")

(def input
  (slurp (io/resource "2020/13.txt")))

(defn invalid?
  [x]
  (= "x" x))

(defn time-to-i
  [x]
  (if (invalid? x)
    x
    (read-string x)))

(defn parse-input
  [i]
  (let [[current-timestamp times] (s/split i #"\n")
        parsed-times            (s/split times #",")]
    [(read-string current-timestamp) (map time-to-i parsed-times)]))

(defn any-busses-leaving-now?
  [target times]
  (first
   (remove nil?
           (for [time times]
             (if (zero? (mod target time))
               time
               nil)))))

(defn find-closest-bus
  [target times]
  (loop [current-offset 0 filtered-times (remove invalid? times)]
    (if-let [next-bus (any-busses-leaving-now? (+ target current-offset) filtered-times)]
      (* next-bus current-offset)
      (recur (inc current-offset) filtered-times))))

(defn part-1
  [i]
  (apply find-closest-bus (parse-input i)))

(defn all-bus-times
  [times current-time]
  [current-time
   (map-indexed (fn[offset time]
                  (if (invalid? time) 0
                      (mod (+ current-time offset) time))) times)])

(defn gt
  [x y]
  (or (invalid? x)
      (invalid? y)
      (> x y)))


(defn gcd
  [a b]
  (if (zero? a)
    b
    (gcd (mod b a) a)))

(defn gcd-range
  [n]
  (let [[f s & t] (sort n)
        initial-gcd (gcd f s)]
    (if (empty? t)
      initial-gcd
      (recur (cons initial-gcd t)))))

(defn lcm
  [n]
  (/ (apply * n) (gcd-range n)))

(defn part-2
  [_ times]
  (let [largest-time             (last (sort (remove invalid? times)))
        position-of-largest-time (count (take-while (partial gt largest-time) times))]
    (first (drop-while (fn[[current-time x]]
                         (not (every? zero? x)))
                       (map (comp (partial all-bus-times times)
                                  (partial + (- position-of-largest-time))
                                  (partial * largest-time))

                           (range))))))


(defn times-and-offsets
  [times]
  (remove nil?
          (map-indexed (fn [position value]
                         (when (not (invalid? value))
                           [position value]))
                       times)))

(defn generate-sets-of-times
  [starting-n times-and-offsets]
  (for [[offset time] times-and-offsets]
    (take 100000 (map (comp (partial + offset) (partial * time) (partial + starting-n)) (range)))))

(defn find-intersections
  [times]
  (apply set/intersection (map set times)))

(defn generate-times
  [_ times]
  (let [offsets (times-and-offsets times)]
    (loop [n 1]
      (let [r (find-intersections (generate-sets-of-times n offsets))]
        (if (empty? r)
          (recur (+ n 100000))
          r)))))

(defn times-are-staggered?
  [times]
  (let [[[_ _ timestamp ] & rest] (sort-by first times)]
    (every? zero?
            (map (fn[[increment minute _]]
                   (mod (+ timestamp increment) minute)) rest))))

(defn increment-lowest-time
  [ts]
  (let [sorted-times                              (sort-by last ts)
        [[offset increment lowest-time] & rest] sorted-times
        [_ _ highest-time]                       (last sorted-times)]
    (conj rest [offset increment (* increment (inc (int (/ (double highest-time) increment))))])))


"How do you count up in the biggest possible increment?"
(defn incremental-solve
  [_ times]
  (let [ts (map (fn[[offset t]] (conj [offset t] t)) (times-and-offsets times))]
    (loop [current-times ts]
      (if (times-are-staggered? current-times)
        current-times
        (recur (increment-lowest-time current-times))))))


(defn debug [t]
  (when (zero? (mod t 10000000))
    (println t)))

(defn factor?
  [times current-time]
  (debug current-time)
  (every? zero?
          (map (fn[[offset t]]
                 (mod (+ offset current-time) t)) times)))

(defn solve-incrementing-largest-number-only
  [_ times]
  (let [ts                                  (times-and-offsets times)
        [[offset largest-increment] & rest] (sort-by (comp - last) ts)]
    [rest
     (first (drop-while (comp not (partial factor? rest))
                        (map (comp (partial + (- offset))
                                   (partial * largest-increment))
                             (range))))]))

(defn return-first-non-factor
  [n times]
  (first
   (remove nil?
           (let [sorted-times (sort-by last times)]
             (for [[offset time] sorted-times]
               (let [remainder (mod n (+ offset time))]
                 (when (pos? remainder)
                   time)))))))

(defn solve-based-on-factors
  [_ times]
  (let [ts                                  (times-and-offsets times)
        [[offset largest-increment] & rest] (sort-by (comp - last) ts)]
    (loop [current-t largest-increment]
      (Thread/sleep 1000)
      (println current-t)
      (if-let [first-non-factor (return-first-non-factor current-t rest)]
        (recur (+ current-t (* largest-increment first-non-factor)))
        current-t))))

"Check the number x, a multiple of the largest value A. If x is divisible by one other number y, but that number is not a factor of A, then we need to increment by more than A - increment by A*y?"

(defn solve-part-2
  []
  (time (apply solve-incrementing-largest-number-only (parse-input input))))


"Find candidate numbers - if we have 17,2,13,19, find multiples of 19 such that 19x-3 is a factor of 17"
"How can 19x-3 be a factor of 17?"

(defn quicker-check
  [x]
  (first
   (filter
    (comp zero? #(mod % 2) (partial + 1))
    (filter
     (comp zero? #(mod % 13) (partial + 2))
     (filter
      (comp zero? #(mod % 17))
      (filter
       (iterate (partial + 19) 19)))))))


(defn filter-check
  [x]
  (first
   (filter
    (comp zero? #(mod % 37) (partial + (- 2)))
    (filter
     (comp zero? #(mod % 47) (partial + (- 1)))
     (filter
      (comp zero? #(mod % 1789) (partial + (- 3)))
      (iterate (partial + 1889) 1889))))))


(defn filter-check
  [x]
  (first
   (filter
    (comp zero? #(mod % 37) (partial + (- 2)))
    (filter
     (comp zero? #(mod % 47) (partial + (- 1)))
     (filter
      (comp zero? #(mod % 1789) (partial + (- 3)))
      (iterate (partial + 1889) 1889))))))

(defn build-filters
  [original-offset ts]
  (fn [v]
    (every? true?
            (map #(% v)
                 (for [[offset multiple] ts]
                   (fn [x] (zero? (mod (+ (- x original-offset) offset) multiple))))))))

(defn general-filter
  [_ times]
  (let [ts                                  (times-and-offsets times)
        [[offset largest-increment] & rest] (sort-by (comp - last) ts)
        filters                             (build-filters offset rest)]
    (first
     (filter filters
             (iterate (partial + largest-increment) largest-increment)))))


(defn search [filters numbers]
    (doall (filter filters numbers)))

(defn parallel-filter
  [_ times]
  (let [ts                                  (times-and-offsets times)
        [[offset largest-increment] & rest] (sort-by (comp - last) ts)
        filters                             (build-filters offset rest)]
    (first (apply concat (pmap (partial search filters)
                               (partition-all 1000 (iterate (partial + largest-increment) (* 100000  largest-increment))))))))


(defn build-quick-filters
  [original-offset times]
  (fn [v]
    (reduce (fn[agg [offset t]]
              (if (= (if (zero? offset) 0 (- t offset)) (rem (- v original-offset) t))
                agg
                (reduced false))) true times)))

(defn search-with-quick-filter
  [_ times]
  (let [ts (times-and-offsets times)
        [[offset largest-increment] & rest] (sort-by (comp - last) ts)
        filters (build-quick-filters offset rest)]
    (first (apply concat (pmap (partial search filters)
                               (partition-all 1000 (iterate (partial + largest-increment)  largest-increment)))))))


(defn print-modular-pattern
  [i]
  (doseq [[offset time] (times-and-offsets i)]
    (println (format "x mod(%d) ≡ %d" time (- time offset)))))
