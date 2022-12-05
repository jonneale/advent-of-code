(ns advent-of-code.2022.4
  (:require [clojure.java.io :as io]))

(def input
  (->> (-> (io/resource "2022/4.txt")
           slurp
           (clojure.string/split  #"\n"))
       (map #(clojure.string/split % #"\,"))))

(defn parse-line
  [line]
  (for [elf line]
    (map read-string (clojure.string/split elf #"-"))))

(defn elves-overlap-completely?
  [line]
  (let [[[first-elf-lower-bound first-elf-upper-bound] [second-elf-lower-bound second-elf-upper-bound]] (parse-line line)]
    (or
     (and (>= first-elf-lower-bound
              second-elf-lower-bound)
          (<= first-elf-upper-bound
              second-elf-upper-bound))
     (and (>= second-elf-lower-bound
              first-elf-lower-bound)
          (<= second-elf-upper-bound
              first-elf-upper-bound)))))

(defn find-complete-overlaps
  [input]
  (count
   (filter elves-overlap-completely? input)))

(defn elves-overlap-at-all?
  [line]
  (let [[[first-elf-lower-bound first-elf-upper-bound] [second-elf-lower-bound second-elf-upper-bound]] (parse-line line)]
    (or

     (and (>= first-elf-lower-bound
              second-elf-lower-bound)
          (<= first-elf-lower-bound
              second-elf-upper-bound))

     (and (>= first-elf-upper-bound
              second-elf-lower-bound)
          (<= first-elf-upper-bound
              second-elf-upper-bound))

     (and (>= second-elf-lower-bound
              first-elf-lower-bound)
          (<= second-elf-lower-bound
              first-elf-upper-bound))

     (and (>= second-elf-upper-bound
              first-elf-lower-bound)
          (<= second-elf-upper-bound
              first-elf-upper-bound)))))

(defn find-partial-overlaps
  [input]
  (count
   (filter elves-overlap-at-all? input)))
