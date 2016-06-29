(ns advent-of-code.24
  (:require [clojure.set :as s]))

(def test-packages
  #{1 2 3 4 5 7 8 9 10 11})
(def test-2
  #{1 2 3 5 6 7})
(def all-packages
  #{1
    2
    3
    7
    11
    13
    17
    19
    23
    31
    37
    41
    43
    47
    53
    59
    61
    67
    71
    73
    79
    83
    89
    97
    101
    103
    107
    109
    113})


(defn all-sets-of-size
  ([set-size packages]
   (all-sets-of-size set-size packages #{}))
  ([set-size packages so-far]
   (if (zero? set-size)
     [so-far]
     (mapcat (fn [next-package]
               (let [new-so-far (conj so-far next-package)]
                 (all-sets-of-size (dec set-size)
                                   (set (filter #(< (last (sort new-so-far)) %) packages))
                                   new-so-far)))
             packages))))

(defn valid-configurations-for-first-trunk
  [packages number-of-trunks]
  (let [total (reduce + packages)]
    (loop [set-size 1]
      (println set-size)
      (let [sets (set (all-sets-of-size set-size packages))]
        (if-let [right-sets (seq (filter #(= total (* number-of-trunks (reduce + %))) sets))]
          right-sets
          (recur (inc set-size)))))))



(defn find-best-package-configuration
  [packages number-of-trunks]
  (first (sort-by #(reduce * %) (valid-configurations-for-first-trunk packages number-of-trunks))))
