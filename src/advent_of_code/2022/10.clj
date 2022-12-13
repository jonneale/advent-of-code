(ns advent-of-code.2022.10
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))
(def input
  (->> (-> (io/resource "2022/10.txt")
           slurp
           (clojure.string/split  #"\n"))))

(def test-input
  ["noop"
   "addx 3"
   "addx -5"])

(def large-test
  (->> (-> (io/resource "2022/10-test.txt")
           slurp
           (clojure.string/split  #"\n"))))

(defn command-number-value
  [s]
  (read-string (second (s/split s #" "))))

(defn simple
  [commands]
  (reduce (fn[agg com]
            (let [last-v (last agg)
                  new-v (conj agg last-v)]
              (if (= "noop" com)
                new-v
                (conj new-v (+ last-v (command-number-value com))))))
          [1] commands))


(defn draw-screen
  [i]
  (doseq [y (range 6)]
    (print "\n")
    (doseq [x (range 0 40)]
      (let [index (min (+ (* y 40) x) (dec (count i)))
            value (nth i index)]
        (if (and (>= value (dec x))
                 (<= value (inc x)))
          (print "#")
          (print "."))))))

(defn solve-part-1
  [i]
  (reduce +
          (let [r (simple i)]
            (for [ss [20 60 100 140 180 220]]
              (* ss (nth r (dec ss)))))))

(defn solve-part-2
  [i]
  (draw-screen (simple i)))
