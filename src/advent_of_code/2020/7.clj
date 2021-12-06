(ns advent-of-code-2020.7
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def test-input
  "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.")

(def test-2
 "shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags." )

(def input (slurp (io/resource "7.txt")))

(defn trim-bag-type
  [bag]
  (s/replace (first (s/split bag #"bag[s]?")) #"[ 0-9\.]+" ""))

(defn trim-bag-description
  [bag]
  [(trim-bag-type bag)
   (read-string (or (re-find #"[0-9]+" bag) "1"))])

(defn line->map
  [line]
  (let [[container contained] (s/split line #"contain")
        all-contained         (map trim-bag-description (s/split contained #","))]
    {(trim-bag-type container) all-contained}))

(defn parse-input
  [i]
  (->>
   (s/split i #"\n")
   (map line->map)
   (apply merge)))

(defn find-all-contained-bags
  [i bagtype]
  (loop [agenda [bagtype] found-bags []]
    (if (seq agenda)
      (let [[h & t] agenda]
        (recur (clojure.set/difference (set (concat t (map first (i h)))) found-bags) (set (conj found-bags h))))
      found-bags)))

(defn search-for-bag-type
  [input bag-type]
  (loop [remaining-bags (sort (keys input)) bags-that-can-contain-bag-type []]
    (if (seq remaining-bags)
      (let [[h & t] remaining-bags
            all-bags (find-all-contained-bags input h)]
        (if ((set all-bags) bag-type)
          (recur t (conj bags-that-can-contain-bag-type h))
          (recur t bags-that-can-contain-bag-type)))
      (remove #(= bag-type %) bags-that-can-contain-bag-type))))

(defn find-bags-containing-bag
  []
  (count (search-for-bag-type (parse-input input) "shinygold")))

(declare count-bags-in-bag)

(defn count-bags
  [input agg [sub-bag-type number-of-bags]]
  (if sub-bag-type
    (+ agg (* number-of-bags (count-bags-in-bag input sub-bag-type)))
    0))

(defn count-bags-in-bag
  [input bag-type]
  (let [bags-in-bag (input bag-type)]
    (if bags-in-bag
      (reduce (partial count-bags input)
              1 bags-in-bag)
      0)))

(defn count-bags-in-shiny-gold-bag
  []
  (dec (count-bags-in-bag (parse-input input) "shinygold")))
