(ns advent-of-code.2020.1
  (:require [clojure.java.io :as io]))


(def input
  (->> (-> (io/resource "2020/1.txt")
           slurp
           (clojure.string/split  #"\n"))
       (map read-string)))
(def target 2020)

(defn find-pair [i target]
  (let [[less-than greater-than]
        (partition-by (fn [x] (< (/ target 2) x)) (sort i))]
    (some (fn [v] (and ((set greater-than) (- target v))
                      (* v (- target v))))
          less-than )))


(defn find-value [i target numbers-allowed]
  (if (= 1 numbers-allowed)
    [((set i) target)]
    (for [v i]
      (cons v (find-value i (- target v) (dec numbers-allowed)) ))))


(defn f
  ([i target n]
   (f i target n []))
  ([i target n agg]
   (if (= 1 n)
     (cons ((set i) target) agg)
     (loop [v i]

       (f i (- target v) (dec n) (cons v agg))))))



(defn find-with-n-values
  [input-list target number-allowed]
  (if (= 1 number-allowed)
    (->> ((set input-list) target) vector (remove nil?) seq)
    (loop [v (sort input-list)]
      (when-let [[h & t] v]
        (if-let [r (find-with-n-values t (- target h) (dec number-allowed))]
          (cons h r)
          (recur t))))))

(apply * (find-with-n-values target 3))

(find-value [1 2 3 4] 7 3)

 (find-it input 2020 2)
(defn quicker
  [i target]
  (some (fn [v] (and ((set i) (- target v))
                       (* v (- target v))))
           i))
