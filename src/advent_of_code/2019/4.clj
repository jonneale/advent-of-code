(ns advent-of-code.2019.4)

(def i-range
  [109165 576723])

(defn digits-increase?
  [[c-x c-y]]
  (<= (Integer/parseInt (str c-x))
      (Integer/parseInt (str c-y))))

(defn no-decreasing-digits?
  [partitioned-char-digits]
  (every? true? (map digits-increase? partitioned-char-digits)))

(defn any-two-digits-identical?
  [partitioned-char-digits]
  (some true? (map (partial apply =) partitioned-char-digits)))

(defn any-two-digits-identical?
  [partitioned-char-digits]
  (some true? (map (partial apply =) partitioned-char-digits)))

(defn valid-password?
  [number]
  (let [partitioned-char-digits (partition 2 1 (str number))]
    (and (no-decreasing-digits? partitioned-char-digits)
         (any-two-digits-identical? partitioned-char-digits))))

(defn any-two-digits-identical-but-not-part-of-larger-group?
  [number]
  (seq (filter #(= 2 (count %)) (vals (group-by identity (str number))))))

(defn valid-password-2?
  [number]
  (let [partitioned-char-digits (partition 2 1 (str number))]
    (and (no-decreasing-digits? partitioned-char-digits)
         (any-two-digits-identical-but-not-part-of-larger-group? number))))

(defn count-valid-values-in-range
  [[lower-bound upper-bound]]
  (count (filter valid-password? (range lower-bound (inc upper-bound)))))

(defn count-valid-values-in-range-2
  [[lower-bound upper-bound]]
  (count (filter valid-password-2? (range lower-bound (inc upper-bound)))))
