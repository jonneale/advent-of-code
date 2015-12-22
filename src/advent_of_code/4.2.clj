(ns advent-of-code.4.2
  (:require [digest :as d]))

(def secret-key
  "iwrupvqb")

(defn find-hash
  []
  (loop [v 1]
    (if (.startsWith (d/md5 (str secret-key v)) "00000")
      v
      (recur (inc v)))))

(defn find-hash-regex
  []
  (loop [v 1]
    (if (re-find #"^00000.*" (d/md5 (str secret-key v)))
      v
      (recur (inc v)))))

(defn find-hash-pre-declared-regex
  []
  (let [p #"^00000.*"]
    (loop [v 1]
      (if (re-find p (d/md5 (str secret-key v)))
        v
        (recur (inc v))))))

(defn find-hash-index
  []
  (loop [v 1]
    (let [result (d/md5 (str secret-key v))]
      (if (and (= \0 (nth result 0))
               (= \0 (nth result 1))
               (= \0 (nth result 2))
               (= \0 (nth result 3))
               (= \0 (nth result 4)))
        v
        (recur (inc v))))))

(defn timing-fn
  []
  (loop [v 1]
    (if (> v 1000000)
      "done"
      (do (d/md5 (str secret-key v))
          (recur (inc v))))))
