(ns advent-of-code.4)

(def secret-key
  "iwrupvqb")

(defn- md5-hash
  [input-string]
  (.digest (java.security.MessageDigest/getInstance "MD5")
           (.getBytes input-string)))

(defn pattern-fn
  "faster than string comparison but still probably way too slow"
  [leading-zeroes]
  (fn [hash]
    (let [hash-values (take (/ leading-zeroes 2) hash)
          last-value (last hash-values)]
      (if (even? leading-zeroes)
        (every? zero? hash-values)
        (and (or (zero? last-value)
                 (and (< 0 last-value) (> 10 last-value)))
             (every? zero? (butlast hash-values)))))))

(defn- calculate-first-hash-matching-pattern
  [leading-zeroes]
  (let [pf (pattern-fn leading-zeroes)]
    (loop [counter 0]
      (let [hash (md5-hash (str secret-key counter))]
        (if (pf hash)
          counter
          (recur (inc counter)))))))

(defn first-starting-with-5-zeroes
  []
  (calculate-first-hash-matching-pattern 5))

(defn first-starting-with-6-zeroes
  []
  (calculate-first-hash-matching-pattern 6))
