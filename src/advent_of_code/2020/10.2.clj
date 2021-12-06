(ns advent-of-code.10.2
  (:require [clojure.java.io :as io]))

(def my-test
  [1
   2
   3
   6
   7
   8
   9
   10
   13])

;; 0 1 2 3 6   7 8 9 10 13
;; 0 1 3 6     7 8 9 10 13
;; 0 2 3 6     7 8 9 10 13
;; 0 3 6       7 8 9 10 13

;; 4

;; 6 7 8 9 10 13
;; 6 7 9 10 13
;; 6 7 10 13
;; 6 8 9 10 13
;; 6 8 10 13
;; 6 9 10 13


(def input

  [28
   33
   18
   42
   31
   14
   46
   20
   48
   47
   24
   23
   49
   45
   19
   38
   39
   11
   1
   32
   25
   35
   8
   17
   7
   9
   4
   2
   34
   10
   3])

;; (defn buid-chain
;;   ([i]
;;    (build-chain [] i))
;;   ([i remaining]
;;    (if (empty? remaining)
;;      i
;;      (let [[h & t] remaining
;;            last-adapter (last i)]
;;        ))
;;    ))


(def small-input
  [16
   10
   15
   5
   1
   11
   7
   19
   6
   12
   4])

(defn calculate-differences
  [adapters]
  (let [sorted-adapters (sort adapters)
        adapters-with-first-and-last-added (conj(into [0] sorted-adapters) (+ 3 (last sorted-adapters)))]
    (first
     (reduce (fn[[differences previous] next-adapter]
               (let [difference (- next-adapter previous)]
                 [(conj differences difference) next-adapter]))
             [[] 0] adapters-with-first-and-last-added))))

(def day-10-input
  (slurp (io/resource "10.txt")))

(defn part-1-solver
  [input]
  (let [differences (calculate-differences input)
        grouped-differences (group-by identity differences)]
    (* (count (get grouped-differences 1))
       (inc (count (get grouped-differences 3))))))

(defn parse-input
  [input]
  (map read-string (clojure.string/split input #"\n")))


(defn solve-part-1
  []
  (-> day-10-input
      parse-input
      part-1-solver))


;; 1 4 5 6 7 10 11 12 15 16 19
;; 1 4 5 7 10 11 12 15 16 19
;; 1 4 5 7 10 12
;; 1 4 6 9
;; 1 4 5 6 9

;; 4 5 6 7
;; (0), 1, 4, 5, 6, 7,     10, 11, 12, 15, 16, 19, (22)
;; (0), 1, 4, 5, 6, 7,     10, 12, 15, 16, 19, (22)
;; (0), 1, 4, 5, 7,        10, 11, 12, 15, 16, 19, (22)
;; (0), 1, 4, 5, 7,        10, 12, 15, 16, 19, (22)
;; (0), 1, 4, 6, 7,        10, 11, 12, 15, 16, 19, (22)
;; (0), 1, 4, 6, 7,        10, 12, 15, 16, 19, (22)
;; (0), 1, 4, 7, 10, 11, 12, 15, 16, 19, (22)
;; (0), 1, 4, 7, 10, 12, 15, 16, 19, (22)
;; 2^3
(defn calculate-number-of-options
  "Look for patterns in differences. 3s are terminators. 2 1s mean there are 2 options. 3 1s mean there are 3 options. 2 1s mean there are 2 options. Up stream they act as multiples - multiply them all together. 3 in a row is actually 2 pairs of 2"
  [adapters]
  (remove (fn[x] (contains? (set x) 3))
          (partition-by identity (rest (calculate-differences adapters)))))

(defn perform-calculation
  [input]
  (reduce (fn[agg v]
            (cond (= 4 v)
                  (* agg 7)
                  (= 3 v)
                  (* agg 4)
                  (= 2 v)
                  (* agg 2)
                  :else
                  agg))
          1 (map count (calculate-number-of-options input))))

(defn solve-part-2
  []
  (-> day-10-input
      (parse-input)
      (perform-calculation)))

;; 1 2 3 4 - 3 in a row. 2 pairs of 2 - 1 2, 1 3, 2 3, 2 4 = 4

;; 3 * 3 + 2




;; 1 2 3 4 5 - 4 in a row. 1 2, 1 3, 1 4,
;;                         2 3, 2 4, 2 5,
;;                         3 4, 3 5, 3 4 5

;;                         3 * 2 pairs of 2

;; 5 consecutive (1111)
;; 3 * 2 + 2
;; ways to reach 5
;; 1 2 3 4 5
;; 1 2 3 5
;; 1 2 4 5
;; 1 3 4 5
;; 1 3 5
;; 1 4 5
;; 2 3 4 5
;; 2 3 5
;; 2 4 5
;; 2 5
;; 3 4 5
;; 3 5


;; 1 2 3 4 5 - 3 * 3 * 2 pairs of 2

;; (* 12 12 2 1 12 1 12)
;; ;; 4^4^2^3^4^4

;; 4 = 12
;; 3 = 4
;; 2 = 2


;; 1 4 5 6 9

;; 1 4 6 9
;; 1 4 5 6 9

;; 1 2
;; 1 4

;; 9604
