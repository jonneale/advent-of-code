(ns advent-of-code.17)

(def test-input
  "20
15
10
5
5")

(def input
  (slurp "./resources/17.txt"))

(defn parse-input
  [i]
  (map read-string (clojure.string/split i #"\n")))

(defn add-leading-zeros
  [s number-of-zeroes]
  (reduce (fn [agg _] (str "0" agg)) s (range (count s) number-of-zeroes)))

(defn find-combinations-bit-mask
  [containers]
  (for [bitmask-as-int (range (bit-shift-left 1 (count containers)))]
    (let [bitmask (add-leading-zeros (Integer/toString bitmask-as-int 2) (count containers))]
      (remove nil?
              (map-indexed (fn [i x]
                             (when (= (nth bitmask i) \1)
                               x))
                           containers)))))

(defn find-correct-combinations
  [i max]
  (filter #(= max (reduce + %))
          (find-combinations-bit-mask (parse-input i))))

(defn find-number-of-min-container-combinations
  [i max]
  (first (sort (frequencies (map count (find-correct-combinations i max))))))
