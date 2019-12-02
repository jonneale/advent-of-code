(ns advent-of-code.2019.2
  (:require [clojure.string :as s]
            [clojure.java.io :as io]))

(def i
  {0 1, 65 10, 70 67, 62 5, 74 5, 110 14, 7 3, 59 59, 86 83, 20 1, 72 1, 58 10, 60 1, 27 27, 1 0, 69 9, 101 99, 24 2, 102 2, 55 55, 85 10, 39 39, 88 1, 46 43, 4 1, 77 10, 106 13, 95 95, 54 6, 92 2, 104 1, 15 3, 48 2, 50 47, 75 75, 99 99, 21 9, 31 31, 32 1, 40 2, 91 91, 108 99, 56 2, 33 31, 13 5, 22 19, 90 87, 109 2, 36 1, 41 39, 89 5, 100 1, 43 43, 61 59, 29 27, 44 1, 93 9, 6 2, 111 0, 28 1, 64 2, 103 103, 51 51, 25 23, 34 6, 17 1, 3 3, 12 1, 2 0, 66 63, 107 0, 23 23, 47 47, 35 35, 82 6, 76 2, 97 95, 19 19, 57 55, 68 2, 11 3, 9 3, 5 1, 112 0, 83 83, 14 0, 45 9, 53 51, 78 75, 26 10, 16 2, 81 79, 79 79, 38 35, 98 5, 87 87, 30 5, 73 71, 96 1, 10 4, 18 6, 105 103, 52 1, 67 67, 71 71, 42 13, 80 1, 37 6, 63 63, 94 91, 8 1, 49 9, 84 2})

(def input
  "1,9,10,3,2,3,11,0,99,30,40,50")

(defn tokenize-input
  [i]
  (apply merge
         (map-indexed (fn [i x] {i (Integer/parseInt x)})
                      (s/split (s/replace i #"\n" "") #","))))

(defn process
  [command i position]
  (let [x-pos (i (+ position 1))
        y-pos (i (+ position 2))
        x (i x-pos)
        y (i y-pos)
        output (i (+ position 3))]
    (assoc i output (command x y))))

(defn process-input
  ([i]
   (process-input i 0))
  ([i position]
   (let [command (i position)]
     (cond (= command 1)
           (recur (process + i position) (+ position 4))
           (= command 2)
           (recur (process * i position) (+ position 4))
           (= command 99)
           i))))

(defn run
  []
  (get (process-input
        (-> (tokenize-input (slurp (io/resource "2019/2.txt")))
            (assoc 1 12)
            (assoc 2 2)))
       0))


(defn find-correct-inputs
  []
  (let [target 19690720
        input (tokenize-input (slurp (io/resource "2019/2.txt")))]
    (loop [x 0 y 0]
      (if (and (> x 99) (> y 99))
        "No answer"
        (let [modified-input (-> input
                                 (assoc 1 x)
                                 (assoc 2 y))
              output         (get (process-input modified-input) 0)]
          (cond (= output target)
                [x y]
                (= x 99)
                (recur 0 (inc y))
                :else
                (recur (inc x) y)))))
    ))
