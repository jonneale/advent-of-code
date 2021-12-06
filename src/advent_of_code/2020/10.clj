(ns advent-of-code-2020.10
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def input (slurp (io/resource "10.txt")))

(def test-input
  "16
10
15
5
1
11
7
19
6
12
4")

(def my-test "3
4
5
6
8
9
12")

(def bigger-test
  "28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3")

(defn parse-input [i]
  (map read-string (s/split i #"\n")))

(defn count-combinations
  [values]
  (if (= (count values) 3)
    4
    (count values)))

(defn find-stand-alone-adapters
  [starting-adapter remaining-adapters possible-next-adapters]
  (filter #(and (not= starting-adapter %)
                (some (fn [offset]
                        ((set remaining-adapters)
                         (- % offset)))
                      (range 1 4)))
          possible-next-adapters))


(defn pretty-print-state
  [a b c r]

  (println)
  (println)
  (println "-----------------------------------------------")
  (println "Routes so far: " r)
  (println "next possibles: " a)
  (println "min-possible: " b)
  (println "stand-alones: " c)
  (println "-----------------------------------------------")
  (Thread/sleep 3000))



(defn count-possible-paths
  [parsed-input]
  (let [target (reduce max parsed-input)]
    (loop [joltage target remaining-adapters (conj parsed-input 0) routes-so-far 1]
      (if (= [0] remaining-adapters)
        routes-so-far
        (let [next-possible-adapters          (filter #((set remaining-adapters) %) 
                                                      (map (partial - joltage) (range 1 4)))
              min-possible-adapter            (reduce min next-possible-adapters)
              adapters-that-could-stand-alone (find-stand-alone-adapters min-possible-adapter remaining-adapters next-possible-adapters)]
          (pretty-print-state next-possible-adapters min-possible-adapter adapters-that-could-stand-alone routes-so-far)
          (recur
           min-possible-adapter
           (remove #(> % min-possible-adapter) remaining-adapters)
           (* routes-so-far
              (+ (count-combinations next-possible-adapters)
                 (reduce + (map (fn[x] (if (some #(> % x) adapters-that-could-stand-alone) 2 1)) adapters-that-could-stand-alone))
                 ))))))))

(defn chain-adapters
  [parsed-input]
  (loop [joltage 0 remaining-adapters parsed-input differences []]
    (if (seq remaining-adapters)
      (let [next-adapter (some #((set remaining-adapters) %)
                               (sort (map (partial + joltage) (range 1 4))))]
        (recur next-adapter
               (remove #(= % next-adapter) remaining-adapters)
               (conj differences (- next-adapter joltage))))

      (let [grouped-differences (group-by identity differences)]
        (* (count (grouped-differences 1))
           (inc (count (grouped-differences 3))))))))




(defn possible-adapters-from-this-joltage
  [joltage input]
  (count (remove nil? (map #((set input) (- joltage %)) (range 1 4)))))

(defn options-from-every-value
  [input]
  (reduce (fn [agg adapter] (assoc agg adapter (possible-adapters-from-this-joltage adapter input))) {} input))

(defn foo
  [i]
  (let [input (parse-input i)
        options-map (options-from-every-value input)
        starting-value (reduce max input)
        required-multiples (remove #(or (<= (options-map %)
                                            1)
                                        ((set input) (+ % 1))
                                        ((set input) (+ % 2))) input)
        optional-multiples (filter #(and (> (options-map %)
                                            1)
                                         (or((set input) (+ % 1))
                                            ((set input) (+ % 2)))) input)  ]
    (+ (apply *
              (map (partial get options-map) required-multiples))
       (apply +
              (map (partial get options-map) optional-multiples)))))
