(ns advent-of-code.2018.1.1)

(defn parse-input
  []
  (map (fn [i] (Integer/parseInt i))
       (clojure.string/split
        (slurp "/Users/jon.neale/scratch/advent-of-code/resources/2018/1.txt") #"\n")))

(defn calculate-freq
  []
  (reduce +
          (parse-input)))

(defn calculate-repeated-freq
  [input]
  (reduce (fn [agg v] (if (contains? agg v)
                        (reduced v)
                        (conj agg v)))
          #{}
          (reductions + (cycle input))))

(defn repeated-freq
  []
  (calculate-repeated-freq (parse-input)))
