(ns advent-of-code.google-puzzler)

(def synonyms [["rate" "ratings"] ["approval" "popularity"]])

(def queries
  [["obama approval rate" "obama popularity ratings"]
   ["obama approval rates" "obama popularity ratings"]
   ["obama approval rate" "popularity ratings obama"]])

(defn synonym-map
  [syns]
  (reduce (fn [agg [word-a word-b]]
            (-> agg
                (assoc word-a word-b)
                (assoc word-b word-a)))
          {}
          syns))

(def all-synonyms
  (synonym-map synonyms))

(defn generate-possible-meanings
  [s]
  (map (fn [word] (sort (remove nil? (into [word] [(all-synonyms word)])))) (clojure.string/split s #" ")))

(defn check-query
  [[string-a string-b]]
  (= (generate-possible-meanings string-a)
     (generate-possible-meanings string-b)))

(defn check-strings
  [strings]
  (map check-query strings))
