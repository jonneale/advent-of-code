(ns advent-of-code.10)

(def input
  "3113322113")

(def test-input
  "111221")

(defn look-and-say
  [input]
  (apply str (mapcat #(str (count %) (first %))
                     (partition-by identity input))))

(defn iterative-look-and-say
  [input n]
  (count (last (take (inc n) (iterate look-and-say input)))))
