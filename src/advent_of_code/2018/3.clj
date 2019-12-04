(ns advent-of-code.2018.3
  (:require [clojure.string :as s]
            [clojure.java.io :as io]))

(def data (slurp (io/resource "2018/3.txt")))

(defn parse-data
  [data]
  (map (fn [line]
         (let [[id _ coords dimensions] (s/split line #"\s")]
           [id
            (map #(Integer/parseInt %) (s/split (apply str (butlast coords)) #","))
            (map #(Integer/parseInt %) (s/split dimensions #"x"))]))
       (s/split data #"\n")))

(defn generate-coordinates-for-claim
  [[x y] [width height]]
  (set (for [x-offset (range width)
             y-offset (range height)]
         [(+ x (inc x-offset))
          (+ y (inc y-offset))])))

(defn count-overlapping-squares
  [data]
  (let [claims (map #(apply generate-coordinates-for-claim (rest %)) data)]
    (reduce (fn [[claimed-squares duplicate-squares] claim]
              [(into claimed-squares claim)
               (into duplicate-squares (clojure.set/intersection claimed-squares claim))])
            [#{} #{}]
            claims)))

(defn sum-overlapping-squares
  [raw-data]
  (count (last (count-overlapping-squares (parse-data raw-data)))))


(defn find-claim-with-no-overlapping-squares
  [raw-data]
  (let [data                  (parse-data raw-data)
        claims                (map (fn [[id & claim]] [id (apply generate-coordinates-for-claim claim)]) data)
        all-coords            (reduce concat (map (comp set last) claims))]
    (some (fn [[id claim]] (empty? (clojure.set/intersection all-coords claim))) claims)))

(defn overlapping?
  [[first-id first-claim] [second-id second-claim]]
  (and (not= first-id second-id)
       (not (empty? (clojure.set/intersection first-claim second-claim)))))

(defn find-claim-with-no-overlapping-squares-2
  [raw-data]
  (let [data                  (parse-data raw-data)
        claims                (map (fn [[id & claim]] [id (apply generate-coordinates-for-claim claim)]) data)]
    (loop [possibles claims]
      (when (zero? (mod (count possibles) 100)) (println "remaining " (count possibles)))
      (when (not (empty? possibles))
        (let [candidate (first possibles)
              remaining-possibles (remove (partial overlapping? candidate) (rest possibles))]
          (if (= remaining-possibles (rest possibles))
            (if (some (partial overlapping? candidate) claims)
              (recur (rest possibles))
              (first candidate))
            (recur remaining-possibles)))))))
