(ns advent-of-code-2020.6
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn parse-input
  []
  (s/split (slurp (io/resource "6.txt"))
            #"\n\n"))


(defn declarations-per-group
  [i]
  (for [passenger-group i]
    (->>
     (-> passenger-group
         (s/replace #"\n" "")
         sort)
     (partition-by identity)
     count)))

(defn unanimous-declarations-per-group
  [i]
  (for [passenger-group i]
    (count (apply clojure.set/intersection (map set (s/split passenger-group #"\n"))))))

(defn count-declarations
  []
  (apply + (declarations-per-group (parse-input))))

(defn count-unanimous-declarations
  []
  (apply + (unanimous-declarations-per-group (parse-input))))
