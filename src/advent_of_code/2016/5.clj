(ns advent-of-code.2016.5
  (:import java.security.MessageDigest
           java.math.BigInteger))


(defn md5
  [seed index]
  (let [s (str seed index)
        algorithm (MessageDigest/getInstance "MD5")
        size (* 2 (.getDigestLength algorithm))
        raw (.digest algorithm (.getBytes s))
        sig (.toString (BigInteger. 1 raw) 16)]
    sig))

(defn valid-md5? 
  [seed index]
  (<= (count (md5 seed index)) 27))


(defn nth-character
  [seed n index]
  (let [hash (md5 seed index)]
    (if (< (count hash) (- 32 (inc n))) 0 (nth hash (- )))))

(def test-seed "abc")

(def seed "reyedfim")

(defn next-valid-hash
  [starting-index seed]
  (loop [i starting-index]
    (if (valid-md5? seed i)
      i
      (recur (inc i)))))

(defn find-hash-indexes
  [seed done-fn]
  (loop [indexes [0]]
    (println indexes)
    (if (done-fn indexes)
      indexes
      (let [latest-value (inc (last indexes))
            new-value    (next-valid-hash latest-value seed)]
        (recur (conj indexes new-value))))))

(defn password-length-is-8?
  [indexes]
  (= 8 (dec (count indexes))))

(defn format-password
  [seed passwords]
  (apply str (map (partial sixth-character seed) (rest passwords))))

(defn naive-password
  [seed]
  (format-password (find-hash-indexes seed password-length-is-8?)))

(defn password-letter-in-every-space?
  [seed passwords]
  (let [found-values (into #{} (map (partial nth-character seed 5) passwords))]
    (empty? (clojure.set/difference (set (apply str (range 1 9))) found-values))))

(defn format-jumbled-password
  [seed indexes]
  (reduce (fn [agg index]
            (if (agg (nth-character seed 5 index))
              agg
              (assoc agg (nth-character seed 5 index) (nth-character seed 6 index)))) 
          {} indexes))
(defn password-by-position
  [seed]
  (find-hash-indexes seed (partial password-letter-in-every-space? seed)))
