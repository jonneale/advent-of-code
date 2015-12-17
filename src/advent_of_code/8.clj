(ns advent-of-code.8
  (:require [clojure.string :as s]))

(defn hex->char
  [x]
  (str (char (Integer/parseInt x 16))))

(def input
  (s/split (slurp "resources/8.txt") #"\n"))

(def test-input
  ["\"\""
   "\"abc\""
   "\"aaa\\\"aaa\""
   "\"\\x27\""])

(defn parse-line
  [x]
  (read-string (s/replace x #"\\x([a-f0-9]{2})" (comp hex->char last))))

(defn literal-lengths
  [strings]
  (map count strings))

(defn in-memory-lengths
  [strings]
  (map (comp count parse-line) strings))

(defn encoded-lengths
  [strings]
  (map (comp count pr-str) strings))

(defn overall-length-differences
  [strings]
  (reduce + (map - (literal-lengths strings) (in-memory-lengths strings))))

(defn overall-encoded-length-differences
  [strings]
  (reduce + (map - (encoded-lengths strings) (literal-lengths strings))))
