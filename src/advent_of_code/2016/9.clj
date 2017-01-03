(ns advent-of-code.2016.9)

(def input
  (slurp "resources/2016/9.txt"))

(defn find-compression
  []
  (map last
       (re-seq #"[^\)]\(([^\)]+)" input)))

(defn expand-compression
  [compression]
  (let [compressed-coefficients (clojure.string/split compression #"x")]
    (reduce + (map read-string compressed-coefficients))))

