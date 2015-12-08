(ns advent-of-code.6
  (:require [clojure.string :as s]))

(def initial-grid
  (apply merge
         (for [x (range 1000)
               y (range 1000)]
           {[x y] 0})))

(def input
  (s/split (slurp "resources/6.txt") #"\n"))

(defn- parse-coords
  [s]
  (map (fn [coord-pair]
         (map #(Integer/parseInt %)
              (s/split coord-pair #",")))
       (remove empty? (s/split s #"([a-z\s])"))))

(defn- parse-line
  [s]
  (let [command-str (s/trim (re-find #"[a-z\s]{7,9}" s))
        coords      (parse-coords s)]
    [command-str coords]))



(defmulti change-light
  (fn [command-str current-value]
    command-str))

;; (defmethod change-light "turn on"
;;   [_ _]
;;   1)

;; (defmethod change-light "turn off"
;;   [_ _]
;;   0)

;; (defmethod change-light "toggle"
;;   [_ value]
;;   (if (zero? value) 1 0))

(defmethod change-light "turn on"
  [_ value]
  (inc value))

(defmethod change-light "turn off"
  [_ value]
  (max 0 (dec value)))

(defmethod change-light "toggle"
  [_ value]
  (+ 2 value))


(defn generate-coord-range
  [[[from-x from-y] [to-x to-y]]]
  (for [x (range from-x (inc to-x))
        y (range from-y (inc to-y))]
    [x y]))

(defn process-single-instuction
  [grid instruction-str]
  (let [[command-str coordinates] (parse-line instruction-str)]
    (reduce (fn [grid coordinate]
              (update-in grid [coordinate] (partial change-light command-str)))
            grid
            (generate-coord-range coordinates))))

(defn- process-instructions
  [input grid]
  (reduce
   process-single-instuction
   grid input))

(defn count-how-many-lights-are-on
  [input grid]
  (->> grid
       (process-instructions input)
       vals
       (reduce +)))
