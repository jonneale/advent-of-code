(ns advent-of-code.19)

(def test-input
  (clojure.string/split "H => HO
H => OH
O => HH" #"\n"))

(def test-str "HOH")

(defn parse-input
  [input]
  (map
   #(let [[from _ to] (clojure.string/split % #" ")]
      [from to])
   input))

(defn find-replacements
  [input s]
  (for [[from to] input]
    (re-seq)
    (if (= from (str char))
      )
    ))
