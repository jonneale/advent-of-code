(ns advent-of-code.2016.5-2
  (:import java.security.MessageDigest
           java.math.BigInteger)
  (:require [digest]))

(defn canonical-md5
  [seed index]
  (digest/md5 (str seed index)))

(defn valid-canonical-md5?
  [md5]
  (.startsWith md5 "00000"))

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

(defn valid-format?
  [seed i]
  (re-matches #"[0-7].*" (quick-md5 seed i)))

(defn quick-next-valid-hash
  [starting-index seed]
  (loop [i starting-index]
    (if (and (quick-valid-md5? seed i)
             (valid-format? seed i))
      i
      (recur (inc i)))))

(defn canonical-next-valid-hash
  [starting-index seed]
  (loop [i starting-index]
    (let [hash (canonical-md5 seed i)]
      (if (valid-canonical-md5? hash)
        i
        (recur (inc i))))))

(defn find-hash-indexes
  [seed done-fn]
  (loop [indexes [0]]
    (if (done-fn indexes)
      indexes
      (let [latest-value (inc (last indexes))
            new-value    (quick-next-valid-hash latest-value seed)]
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
  (format-password seed (find-hash-indexes seed password-length-is-8?)))

(defn print-password
  [password]
  (let [p (str (or (password 0) "*")
               (or (password 1) "*")
               (or (password 2) "*")
               (or (password 3) "*")
               (or (password 4) "*")
               (or (password 5) "*")
               (or (password 6) "*")
               (or (password 7) "*"))]
    (println "----------------")
    (println (str "|   " p "   |"))
    (println "----------------")))

(defn invalid-position?
  [position]
  (nil? (re-matches #"[0-7].*" (str position))))

(defn find-unordered-password
  ([seed]
   (find-unordered-password seed 0))
  ([seed starting-index]
   (loop [i starting-index current-password {0 nil 1 nil 2 nil 3 nil 4 nil 5 nil 6 nil 7 nil}]
     (println "CRACKING NEXT CHARACTER")
     (print-password current-password)
     (if-not (some nil? (vals current-password))
       (apply str (vals current-password))
       (let [next-valid-md5 (canonical-next-valid-hash i seed)
             _   (println "CRACKING SUCCESSFUL. LEET HACKING D00D.")
             _   (println "Found hash for seed " next-valid-md5 " " (canonical-md5 seed next-valid-md5))
             hashed-value   (apply str (drop 5 (canonical-md5 seed next-valid-md5)))
             [position value & _] hashed-value]
         (if (or (invalid-position? position) (current-password (Integer/parseInt (str position))))
           (recur (inc next-valid-md5) current-password)
           (recur (inc next-valid-md5) (assoc current-password (Integer/parseInt (str position)) value))))))))
