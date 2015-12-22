(ns advent-of-code.16)

(def input
  (clojure.string/split (slurp "./resources/16.txt") #"\n"))

(defn to-map
  [i]
  (apply merge (map (fn [[k v]] {(keyword k) (read-string v)})
              (partition 2 i))))

(defn parse-line
  [i]
  (to-map
   (-> i
       (clojure.string/replace #"[^\w\s]" "")
       (clojure.string/split #"\s"))))

(defn parse-input
  []
  (map parse-line input))

(def match-data
  {:children 3
   :cats 7
   :samoyeds 2
   :pomeranians 3
   :akitas 0
   :vizslas 0
   :goldfish 5
   :trees 3
   :cars 2
   :perfumes 1})
15gg
(def key-fn
  {:cats <
   :trees <
   :pomeranians >
   :goldfish >})

(defn matches?
  [k v sue]
  (let [f (or (key-fn k) =)]
    (or (not (k sue)) (f v (k sue)))))

(defn find-sue
  []
  (reduce (fn [agg [key value]]
            (filter #(matches? key value %) agg))
          (parse-input) match-data))

(defn find-real-sue
  []
  (reduce (fn [agg [key value]]
            (filter #(matches? key value %) agg))
          (parse-input) match-data))
