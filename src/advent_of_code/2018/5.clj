(ns advent-of-code.2018.5
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.string :as str]))

(def data  (s/trim (str (slurp (io/resource "2018/5.txt")))))

(def replacements
  (java.util.regex.Pattern/compile
   (s/join "|"
           (reduce concat (for [character (map char (range 97 123))]
                            [(str character (s/upper-case character))
                             (str (s/upper-case character) character)])))))

(def removals (for [character (map char (range 97 123))]
                (str character (s/upper-case character))))

(defn change-string-until-no-more-changes
  [to-change c]
  (let [updated-string (s/replace to-change replacements "")]
    (if (= updated-string to-change)
      (or c 0)
      (recur updated-string (inc (or c 0))))))

(def char-lookup
  (apply merge (for [character (map char (range 97 123))]
                 {character (first (s/upper-case (str character)))})))

(defn fast-react
  [s agg]
  (let [[first-character & remaining-string] s]
    (cond (nil? first-character)
          (apply str agg)
          (empty? agg)
          (recur remaining-string [first-character])
          (= first-character (first agg))
          (recur remaining-string (cons first-character agg))
          (= (get char-lookup first-character first-character)
             (get char-lookup (first agg) (first agg)))
          (recur remaining-string (rest agg))
          :else
          (recur remaining-string (cons first-character agg)))))

(defn find-best-reactor
  [to-change]
  (first
   (sort-by last
            (map (fn [[first-char second-char]]
                   (println "Trialing " first-char "/" second-char)
                   [first-char
                    (count (fast-react
                            (s/replace (s/replace data (str first-char) "")
                                       (str second-char) "")
                            []))])
                 removals))))
