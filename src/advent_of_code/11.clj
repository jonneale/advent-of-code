(ns advent-of-code.11)

(def input
  "vzbxkghb")

(def max-char-value (int \z))

(def min-char-value (int \a))

(defn generate-next-password-ints
  [old-password]
  (let [int-values (map int old-password)]
    (loop [current-iteration int-values password-so-far []]
      (if (empty? current-iteration)
        password-so-far
        (let [updated-last-letter (inc (last current-iteration))
              other-letters (butlast current-iteration)]
          (if (> updated-last-letter max-char-value)
            (recur other-letters (cons min-char-value password-so-far))
            (concat other-letters (cons updated-last-letter password-so-far))))))))

(defn next-password
  [old-password]
  (apply str (map char (generate-next-password-ints old-password))))

(defn consecutive?
  [three-chars]
  (let [ints (map int three-chars)]
    (every? #(= 1 %) (map - (rest ints) ints))))

(defn two-overlapping-pairs?
  [password]
  (->> password
       (partition 2 1)
       (filter (fn [[x y]] (= x y)))
       set
       count
       (< 1)))

(defn invalid?
  [password]
  (or (not (some consecutive? (partition 3 1 password)))
      (some (set password) #{\i \o \l})
      (not (two-overlapping-pairs? password))))

(defn next-valid-password
  [old-password]
  (first (drop-while invalid? (rest (iterate next-password old-password)))))
