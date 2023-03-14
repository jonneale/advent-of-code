(ns advent-of-code.2022.25
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def example
  (s/split "1=-0-2
12111
2=0=
21
2=01
111
20012
112
1=-1=
1-12
12
1=
122" #"\n"))

(def input
    (->> (-> (io/resource "2022/25.txt")
              slurp
             (clojure.string/split  #"\n"))))


(def snafu->number
  {\= -2
   \- -1
   \0 0
   \1 1
   \2 2})

(def number->snafu
  (apply merge (map hash-map (vals snafu->number) (keys snafu->number))))

(defn process-number
  [number-string]
  (reduce +
          (map-indexed (fn[i x]
                         (bigint (* (Math/pow 5 i)
                                    (snafu->number x))))
                       (reverse number-string))))

(defn to-snafu
  [l]
  (apply str (map number->snafu l)))

(defn sum-list-of-numbers
  [l]
  (reduce + (map process-number l)))

(defn to-base-5
  [x]
  (.toString (biginteger x) 5))

(defn snafu-ise
  [x]
  (let [[agg carry]
        (reduce (fn [[agg carry] v]
                  (let [value (+ carry v)]
                    (cond (> 3 value)
                          [(conj agg value) 0]
                          (= 3 value)
                          [(conj agg "=") 1]
                          (= 4 value)
                          [(conj agg "-") 1]
                          :else
                          [(conj agg "0") 1])))
                [[] 0] x)]
    (conj agg carry)))

(defn sp
  [x]
  (->> x
       (to-base-5)
       (map (comp read-string str))
       reverse))

(defn parse-to-snafu
  [x]
  (->> x
       (to-base-5)
       (map (comp read-string str))
       reverse
       snafu-ise
       reverse
       (apply str)
       (drop-while #(= \0 %))
       (apply str)))


(defn test-translations
  [x]
  (map #(vector % [(= % (-> % parse-to-snafu process-number int))]) (range x)))

(defn solve-part-1
  []
  (-> input
      sum-list-of-numbers
      parse-to-snafu))
