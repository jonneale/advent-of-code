(ns advent-of-code-2020.5
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn parse-input
  []
  (clojure.string/split (slurp (io/resource "5.txt"))
                        #"\n"))

(defn -find-seat
  [instructions min-row max-row min-column max-column]
  (let [row-diff    (int (/ (- max-row min-row) 2))
        column-diff (int (/ (- max-column min-column) 2))
        [h & t] instructions]
    (case h
      \F (recur t min-row (+ min-row row-diff) min-column max-column)
      \B (recur t (- max-row row-diff) max-row min-column max-column)
      \L (recur t min-row max-row min-column (+ min-column column-diff))
      \R (recur t min-row max-row (- max-column column-diff) max-column)
      (+ (* max-row 8) max-column))))

(defn find-seat
  [instructions]
  (-find-seat instructions 0 127 0 7))

(defn find-missing-seat
  []
  (let [all-seats-so-far (set (map (partial find-seat) (parse-input)))]
    (clojure.set/difference (set (range (reduce min all-seats-so-far)
                                        (reduce max all-seats-so-far)))
                            all-seats-so-far)))

(defn find-max-boarding-pass
  []
  (reduce max (map (partial find-seat) (parse-input))))
