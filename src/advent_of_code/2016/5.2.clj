(ns advent-of-code.2016.5-2
  (:import java.security.MessageDigest
           java.math.BigInteger))


(defn quick-md5
  [seed index]
  (let [s (str seed index)
        algorithm (MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (.toString (BigInteger. 1 raw) 16)))

(defn quick-valid-md5? 
  [seed index]
  (<= (count (quick-md5 seed index)) 27))

(defn md5 
  [seed index]
  (let [hash    (quick-md5 seed index)
        algorithm (MessageDigest/getInstance "MD5")
        size (* 2 (.getDigestLength algorithm))
        padding (apply str (repeat (- size (count hash)) "0"))]
    (str padding hash)))

(defn quick-next-valid-hash
  [starting-index seed]
  (loop [i starting-index]
    (if (quick-valid-md5? seed i)
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
  (let [hashes (map (partial md5 seed) (rest passwords))]
    (apply str (map #(nth % 5) passwords))))

(defn naive-password
  [seed]
  (format-password (find-hash-indexes seed password-length-is-8?)))
