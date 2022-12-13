(ns advent-of-code.2022.13
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))
(def input
    (->> (-> (io/resource "2022/13.txt")
              slurp
              (clojure.string/split  #"\n\n"))
         (map #(clojure.string/split % #"\n"))
         (map (fn[x] (map #(read-string %) x)))))

(declare compare-pair)

(defn compare-element
  [first-element second-element]
  (cond (and (number? first-element) (number? second-element))
        (<= first-element second-element)
        (number? first-element)
        (compare-pair (vector first-element) second-element)
        (number? second-element)
        (compare-pair first-element (vector second-element))
        (and (seq? first-element (seq? second-element)))
        (compare-pair first-element second-element)))

(declare compare-elements)

(defn compare-pair?
  [first-element second-element]
  (cond (and first-element (not second-element))
        :false
        (and (not first-element) second-element)
        :true
        (and (not first-element) (not second-element))
        :next
        (and (number? first-element)
             (number? second-element))
        (cond (> first-element second-element)
              :false
              (< first-element second-element)
              :true
              :else
              :next)
        (and (sequential? first-element)
             (sequential? second-element))
        (compare-elements first-element second-element)
        (sequential? first-element)
        (compare-elements first-element (vector second-element))
        :else
        (compare-elements (vector first-element) second-element)))


(defn compare-elements
  [first-element second-element]
  (let [[h-first & h-rest] first-element
        [t-first & t-rest] second-element]
    (if (and (nil? h-first) (nil? t-first))
      :next
      (let [result (compare-pair? h-first t-first)]
        (case (compare-pair? h-first t-first)
          :true :true
          :false :false
          :next (compare-elements h-rest t-rest))))))

(defn sort-fn
  [first-element second-element]
  (if (= :true (compare-elements first-element second-element)) -1 1))

(defn compare-all
  [i]
  (map-indexed (fn [index x] [index (apply compare x)]) i))

(defn sorted-input
  [i]
  (sort sort-fn i))

(defn solve-part-1
  []
  (reduce (fn[agg [ix v]]
            (if (= :true v)
              (+ agg (inc ix))
              agg))
          0
          (compare-all input)))

(defn input-with-decoder-packets
  [input]
  (concat (apply concat input) [[[2]] [[6]]]))

(defn filter-control-elements
  [i]
  (filter (fn[[index v]]
            (or (= v [[2]])
                (= v [[6]])))
          i))

(defn to-numbered
  [i]
  (map-indexed (fn[idx x] [(inc idx) x]) i))

(defn solve-part-2
  [input]
  (apply * (map first (filter-control-elements (to-numbered (sorted-input (input-with-decoder-packets input)))))))
