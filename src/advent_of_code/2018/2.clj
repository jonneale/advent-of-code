(ns advent-of-code.2018.2
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def data (slurp (io/resource "2018/2.txt")))

(defn parse-data
  [data]
  (s/split data #"\n"))

(defn generate-id
  [label]
  (clojure.set/intersection #{2 3} (set (vals (frequencies label)))))

(defn calculate-checksum
  [data]
  (apply * (vals
            (frequencies (reduce concat
                                 (map (comp vec generate-id)
                                      (parse-data data)))))))

(defn to-position-and-letter-map
  [label]
  (set (map-indexed vector label)))

(defn find-similar-boxes
  [data]
  (let [parsed-data (map to-position-and-letter-map (parse-data data))]
    (remove nil?
            (for [candidate-1 parsed-data
                  candidate-2 parsed-data]
              (when (and (not= candidate-1 candidate-2)
                         (= (dec (count candidate-1)) (count (clojure.set/intersection candidate-1 candidate-2))))
                (apply str (map last (sort-by first (clojure.set/intersection candidate-1 candidate-2)))))))))
