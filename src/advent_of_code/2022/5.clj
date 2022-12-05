(ns advent-of-code.2022.5
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def test-input
  "    [D]
[N] [C]
[Z] [M] [P]
 1   2   3

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")
(defn split-lines
  [i]
  (s/split i #"\n"))

(def input
  (->> (-> (io/resource "2022/5.txt")
           slurp)))

(defn format-row
  [row]
  (map second (partition-all 4 row)))

(defn parse-state
  [state]
  (let [labels (remove-spaces (last state))
        towers (butlast state)]
    (apply merge-with concat
           (for [row towers]
             (apply merge
                    (remove nil?
                            (map-indexed (fn [i x] (when (not= \space x) {(nth labels i) [x]})) (format-row row))))))))

(defn parse-instructions
  [instructions]
  (for [instruction instructions]
    (let [[_ number _ from _ to] (s/split instruction #" ")]
      [(first from) (first to) (read-string number)])))

(defn one-move
  [f state [from to number]]
  (let [moved-from (drop number (state from))
        moved-to (concat (f (take number (state from))) (state to))]
    (-> state
        (assoc from moved-from)
        (assoc to moved-to))))

(defn process
  [f state instructions]
  (if (empty? instructions)
    state
    (recur f (one-move f state (first instructions)) (rest instructions))))

(defn parse-input
  [i]
  (let [[raw-state raw-instructions] (map split-lines (s/split i #"\n\n"))]
    [(parse-state raw-state) (parse-instructions raw-instructions)]))

(defn solve
  [i f]
  (let [r  (apply (partial process f) (parse-input i))
        k  (map (comp first str) (range 1 10))]
    (reduce (fn[agg v] (str agg (first (r v)))) "" k)))

(defn solve-part-1
  [i]
  (solve i reverse))


(defn solve-part-2
  [i]
  (solve i identity))
