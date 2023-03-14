(ns advent-of-code.2022.3
  (:require [clojure.java.io :as io]))

(def input
  (->> (-> (io/resource "2022/3.txt")
           slurp
           (clojure.string/split  #"\n"))))

(def alphabet (map (comp char (partial + 97)) (range 26)))

(def full-alpha
  (concat alphabet
          (for [letter alphabet]
            (first (clojure.string/upper-case letter)))))

(def alphabet-map
  (apply merge (map-indexed (fn[i x] {x (inc i)}) full-alpha)))

(defn find-items-in-multiple-compartments
  [i]
  (reduce +
          (for [line i]
            (let [half-length (int ( / (count line) 2))
                  first-half (set (take half-length line))
                  second-half (set (drop half-length line))]
              (reduce + (map alphabet-map (clojure.set/intersection first-half second-half)))))))

(defn find-items-common-to-groups
  [i]
  (reduce +
          (for [group (partition 3 i)]
            (alphabet-map (first (apply clojure.set/intersection (map set group)))))))
