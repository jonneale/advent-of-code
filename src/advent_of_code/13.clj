(ns advent-of-code.13
  (:require [clojure.string :as string]))

(def test-input
  (string/split "Alice would gain 54 happiness units by sitting next to Bob.
Alice would lose 79 happiness units by sitting next to Carol.
Alice would lose 2 happiness units by sitting next to David.
Bob would gain 83 happiness units by sitting next to Alice.
Bob would lose 7 happiness units by sitting next to Carol.
Bob would lose 63 happiness units by sitting next to David.
Carol would lose 62 happiness units by sitting next to Alice.
Carol would gain 60 happiness units by sitting next to Bob.
Carol would gain 55 happiness units by sitting next to David.
David would gain 46 happiness units by sitting next to Alice.
David would lose 7 happiness units by sitting next to Bob.
David would gain 41 happiness units by sitting next to Carol."
                        #"\n"))

(def input
  (string/split (slurp "./resources/13.txt") #"\n"))

(defn parse-input
  [i]
  (let [tokens (string/split i #"\s")
        subject (first tokens)
        target  (apply str (butlast (last tokens)))
        operator (if (= (nth tokens 2) "gain") + -)
        amount   (read-string (nth tokens 3))]
    {[subject target] [operator amount]}))

(defn permutations
  [colls]
  (if (= 1 (count colls))
    (list colls)
    (for [head colls
          tail (permutations (disj (set colls) head))]
      (cons head tail))))

(defn apply-score
  [agg score]
  (if score
    (let [[f amount] score]
      (f agg amount))
    agg))

(defn score-arrangement
  [score-criteria arrangement]
  {:arrangement arrangement
   :score
   (reduce
    (fn [agg guest-indexes]
      (let [[left-guest target-guest right-guest] (map #(nth arrangement %) guest-indexes)
            left-score  (score-criteria [target-guest left-guest])
            right-score (score-criteria [target-guest right-guest])]
        (-> agg
            (apply-score left-score)
            (apply-score right-score))))
    0 (partition 3 1 (range (count arrangement)) (range (count arrangement))))})

(defn arrange-people
  [input]
  (let [scores (apply merge (map parse-input input))
        subjects (set (map first (keys scores)))
        all-possible-seating-plans (permutations subjects)]
    (take 1 (reverse (sort-by :score (map (partial score-arrangement scores) all-possible-seating-plans))))))

(defn arrange-people-and-me
  [input]
  (let [scores (apply merge (map parse-input input))
        subjects (conj (set (map first (keys scores))) "Jon")
        all-possible-seating-plans (permutations subjects)]
    (take 1 (reverse (sort-by :score (map (partial score-arrangement scores) all-possible-seating-plans))))))
