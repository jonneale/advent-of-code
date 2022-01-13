(ns advent-of-code.2020.18
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def i1 "1 + 2 * 3 + 4 * 5 + 6" )

(def i2 "1 + (2 * 3) + (4 * (5 + 6))")

(def i3 "2 * 3 + (4 * 5)")

(def i4 "5 + (8 * 3 + 9 + 3 * 4 * 3)")

(def i5 "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")

(def i6 "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")

(def input (slurp (io/resource "2020/18.txt")))

(defn remove-spaces
  [l]
  (s/replace l " " ""))

(declare do-it)

(defn find-balanced-bracketed-expression
  ([l]
   (find-balanced-bracketed-expression l "" false))
  ([l agg opening-bracket-found?]
   (let [[h & t] l
         opening-bracket-count (get (frequencies agg) \( 0)
         closing-bracket-count (get (frequencies agg) \) 0)]
     (cond (and (not opening-bracket-found?) (= \( h))
           (recur t (str h) true)
           (not opening-bracket-found?)
           (recur t agg opening-bracket-found?)
           (and opening-bracket-found? (= opening-bracket-count closing-bracket-count))
           agg
           :else
           (recur t (str agg h) opening-bracket-found?)))))

(defn resolve-brackets
  [maths-fn l]
  (if (re-find #"\(" l)
    (let [first-bracketed-expression (find-balanced-bracketed-expression l)]
      (do-it maths-fn (s/replace-first l first-bracketed-expression (do-it maths-fn (apply str (butlast (rest first-bracketed-expression)))))))
    l))

(defn get-operator
  [c]
  (get {"+" + "-" - "*" * "/" /} (apply str c)))

(defn parse-number
  [c]
  (read-string (apply str c)))

(defn calc
  [s]
  (let [[o1 operator o2] (partition-by #(nil? (re-matches #"[0-9]" (str %))) s)]
    ((get-operator operator) (parse-number o1) (parse-number o2))))


(defn resolve-operation
  [l operators]
  (if-let [matching-group (re-find (re-pattern (str "[0-9]+" operators "[0-9]+")) l)]
    (recur (s/replace-first l matching-group (str (calc matching-group))) operators)
    l))

(defn resolve-multiplication
  [l]
  (resolve-operation l "[/*//]"))

(defn resolve-addition
  [l]
  (resolve-operation l "[/+/-]"))

(defn resolve-maths-no-operator-precedence
  [l]
  (-> l
      (s/replace #"\)" "")
      (resolve-operation "[/+/-/*//]")))

(defn resolve-maths
  [l]
  (-> l
      (s/replace #"\)" "")
      resolve-addition
      resolve-multiplication))

(defn do-it
  [maths-fn l]
  (let [r (if (re-find #"[\(]" l)
            (resolve-brackets maths-fn l)
            (maths-fn l))]
    (if (= r l)
      l
      (recur maths-fn r))))

(defn solve-part-1
  []
  (reduce + (map (comp read-string (partial do-it resolve-maths-no-operator-precedence) remove-spaces) (s/split input #"\n"))))

(defn solve-part-2
  []
  (reduce + (map (comp read-string (partial do-it resolve-maths) remove-spaces) (s/split input #"\n"))))
