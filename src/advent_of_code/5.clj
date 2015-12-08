(ns advent-of-code.5
  (:require [clojure.set :as s]))

(def input (clojure.string/split (slurp "resources/5.txt") #"\n"))

(def vowels #{\a \e \i \o \u})

(def forbidden-strings ["ab" "cd" "pq" "xy"])

(defn- count-vowels
  [s]
  (count (filter #(some vowels #{%}) s)))

(defn- double-letter?
  [s]
  (some true? (map #(apply = %) (partition 2 1 s))))

(defn- forbidden-string?
  [s]
  (some true? (map #(.contains s %) forbidden-strings)))


(defn nice-string-v1?
  [s]
  (and (>= (count-vowels s) 3)
       (double-letter? s)
       (not (forbidden-string? s))))

(defn repeated-but-not-overlapping?
  [s]
  (loop [found-double? false letters s]
    (let [[first-letter second-letter & rest-letters] letters]
      (if (and first-letter second-letter)
        (recur (or found-double? (.contains (apply str rest-letters) (str first-letter second-letter))) (rest letters))
        found-double?))))

(defn- repeats-with-a-gap?
  [s]
  (some true? (map (fn [[x _ y]] (= x y)) (partition 3 1 s))))

(defn nice-string-v2?
  [s]
  (and (repeated-but-not-overlapping? s)
       (repeats-with-a-gap? s)))

(defn count-safe-strings
  [input safe-checker]
  (count (filter safe-checker input)))


;; (count-safe-strings input nice-string-v1?)
