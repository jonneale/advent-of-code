(ns advent-of-code.2022.2
  (:require [clojure.java.io :as io]))

(def shape-points
  {:rock 1
   :paper 2
   :scissors 3})

(def result-points
  {:win 6
   :draw 3
   :loss 0})

(def rules
  {[:rock :rock] :draw
   [:rock :paper] :win
   [:rock :scissors] :loss
   [:scissors :scissors] :draw
   [:scissors :paper] :loss
   [:scissors :rock] :win
   [:paper :paper] :draw
   [:paper :rock] :loss
   [:paper :scissors] :win})


(def rules
  {[:rock :rock] :draw
   [:rock :paper] :win
   [:rock :scissors] :loss
   [:scissors :scissors] :draw
   [:scissors :paper] :loss
   [:scissors :rock] :win
   [:paper :paper] :draw
   [:paper :rock] :loss
   [:paper :scissors] :win})

(def throw-based-on-result
  (apply merge
         (for [[[us them] result] rules]
           {[us result] them})))

(def opponent-mapping
  {"A" :rock
   "B" :paper
   "C" :scissors})

(def input
  (->> (-> (io/resource "2022/2.txt")
           slurp
           (clojure.string/split  #"\n"))
       (map #(clojure.string/split % #" "))))

(defn score-permutation
  [permutation results]
  (for [[opponent our] results]
    (let [opponent-throw (opponent-mapping opponent)
          our-throw (permutation our)
          result (rules [opponent-throw our-throw])
          score  (+ (result-points result) (shape-points our-throw))]
      score)))

(def mapping
  {"X" :rock "Y" :paper "Z" :scissors})

(defn score-part-1
  [i]
  (reduce + (score-permutation mapping i)))

(def result-mapping
  {"X" :loss "Y" :draw "Z" :win})


(defn score-part-2
  [i]
  (reduce +
          (for [[opponent result-letter] i]
            (let [opponent-throw (opponent-mapping opponent)
                  result (result-mapping result-letter)
                  our-throw (throw-based-on-result [opponent-throw result])
                  score (+ (result-points result) (shape-points our-throw))]
              score))))
