(ns advent-of-code.12
  (:require [clojure.data.json :as json]))

(def input (json/read-str (slurp "resources/12.json")))

(def test-input (json/read-str "[1,{\"c\":\"red\",\"b\":2},3]"))

(defn sum-all-numbers
  [input]
  (reduce + (map read-string (re-seq #"-?[0-9]+" input))))

(defn red
  [v]
  (= v "red"))

(declare parse-numbers-out-of-node)

(defn- return-data-at-level
  [node]
  (let [values (vals node)]
    (if (some red values)
      0
      (for [v (vals node)]
        (parse-numbers-out-of-node v)))))

(defn parse-numbers-out-of-data
  [v]
  (cond (number? v)
        v
        (vector? v)
        (map parse-numbers-out-of-data v)
        (string? v)
        0
        :else
        (return-data-at-level v)))
