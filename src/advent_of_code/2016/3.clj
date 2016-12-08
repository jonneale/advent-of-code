(ns advent-of-code.2016.3)

(def parse-input-1
  (for [line (clojure.string/split (slurp "resources/2016-3.txt") #"\n")]
    (map read-string
     (remove empty? (clojure.string/split line #"[^0-9]")))))

(def parse-input-2
  (partition 3 
             (reduce concat 
                     (reduce (fn [[agg-a agg-b agg-c] [a b c]]
                               [(cons a agg-a) (cons b agg-b) (cons c agg-c)])
                             [[] [] []]
                             (partition 3 (map read-string (remove empty? (clojure.string/split (slurp "resources/2016-3.txt") #"[^0-9]"))))))))

(defn valid-triangle?
  [sides]
  (let [[a b c] (sort sides)]
    (> (+ a b) c)))

(defn count-valid-triangles
  [input]
  (count (filter valid-triangle? input)))
