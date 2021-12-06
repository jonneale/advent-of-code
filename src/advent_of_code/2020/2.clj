(ns advent-of-code.2020.2
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def test-input
  "1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc")

(def input
  (->> "2020/2.txt"
       (io/resource)
       slurp))


(defn parse-input [input]
  (for [i (s/split input #"\n")]
    (let [[rules raw-password] (s/split i #": ")
          grouped-password     (group-by identity raw-password)
          [character-count character] (s/split rules #" ")
          [min-count max-count] (map read-string (s/split character-count #"-"))]
      (let [char-count (count (grouped-password (first character)))]
        (and (>= char-count min-count)
             (<= char-count max-count))))))

(defn char-at-position
  [password position]
  (nth password (dec position)))




(defn count-valid-passwords-1 [input]
  (count (filter true?  (parse-input input))))

(defn count-valid-passwords-2 [input]
  (count (filter true?  (parse-input-policy-2 input))))
