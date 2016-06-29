(ns advent-of-code.24
  (:require [clojure.set :as s]))


(defn valid-subs
  [so-far packages total]
  (cond
    (= total (reduce + so-far))
    [so-far]
    (> (+ (reduce + so-far) (reduce min packages)) total)
    []
    :else
    (mapcat (fn [next-package]
              (valid-subs (conj so-far next-package) (disj packages next-package) total))
            (filter #(<= (+ (reduce + so-far) %) total) packages))))


(defn valid-subs-2
  [so-far packages total]
  (cond
    (= total (reduce + so-far))
    [so-far]
    (> (+ (reduce + so-far) (reduce min packages)) total)
    []
    :else
    (mapcat (fn [next-package]
              (valid-subs-2 (conj so-far next-package) (disj packages next-package) total))
            (filter #(and (< (or (last (sort so-far)) 0) %)
                          (<= (+ (reduce + so-far) %)) total) packages))))


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


(def test-packages
  #{1 2 3 4 5 7 8 9 10 11})
(def test-2
  #{1 2 3 5 6 7})

(defn valid-subs-3
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
  (first (sort-by #(reduce * %) (valid-subs-3 packages number-of-trunks))))




(defn foo
  []
  (let [so-far [7]
        packages test-2
        total 8]
    (filter #(and (< (or (last (sort so-far)) 0) %)
                  (<= (+ (reduce + so-far) %)) total) packages)))

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

(defn f
  [x]
  (if (coll? (first x))
    (map f x)
    x))


(defn y
  [p]
  (take 100 (sort-by count (set (map sort (valid-subs-2 [] p (/ (reduce + p) 3)))))))

(defn x
  [p]
  (into #{}
        (remove empty?
                (map #(if (coll? (first %)) (sort (first %)) (sort %))
                     (reduce concat
                             (y p))))))

(defn smallest-qe
  [subset]
  (reduce * subset))

(defn all-valid-lists
  [packages]
  (let [total         (reduce + packages)
        all-subsets   (all-subsets packages)
        valid-subsets (filter #(= (* 3 (reduce + %)) total) all-subsets)
        shortest-subsets (last (first (sort-by key (group-by count valid-subsets))))]
    (sort-by smallest-qe shortest-subsets)))

(defn quick-lists
  [packages]
  (sort-by (comp count first) (all-valid-lists packages)))

(defn all-lists
  [packages]
  (into #{}
        (for [first-bucket (all-subsets packages)
              second-bucket (all-subsets (s/difference packages first-bucket))]
          [first-bucket second-bucket (s/difference  packages (concat first-bucket second-bucket))])))

(defn acceptable-lists
  [packages]
  (sort-by (comp count first) (filter #(apply = (map (partial reduce +) %)) (all-lists packages))))
