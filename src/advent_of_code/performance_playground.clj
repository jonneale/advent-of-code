(ns advent-of-code.performance-playground)

(def measurements
  (vec (let [random (new java.util.Random 2021)]
         (repeatedly 5000000 #(+ 10 (. random nextLong 90))))))


(defn p1
  []
  (->> (for [[a b :as ab] (partition 2 1 measurements)
             :when (< a b)]
         ab)
       count
       time))

(defn p2
  []
  (->> measurements
       (partition 2 1)
       (filter (fn [[a b]] (< a b)))
       count
       time))

(defn p3
  []
  (->> (map < measurements (rest measurements))
       (filter true?)
       count
       time))

(defn p4
  []
  (->> (map < measurements (rest measurements))
       (filter true?)
       (reduce (fn [result item] (inc result)) 0)
       time))

(defn p5
  []
  (->> (map < measurements (rest measurements))
       (reduce (fn [result item] (if (true? item) (inc result) result)) 0)
       time))

(defn p6
  []
  (->> (map < measurements (rest measurements))
       (reduce ((filter true?)
                (fn [result item] (inc result)))
               0)
       time))

(defn p7
  []
  (->> (map < measurements (rest measurements))
       (transduce
        (filter true?)
        (fn
          ([result item] (inc result))
          ([result] result))
        0)
       time))

(defn booleong
  [test]
  (if test 1 0))

(defn p8
  []
  (->> (map < measurements (rest measurements))
              (map booleong)
              (reduce + 0)
              time))


(defn p9
  []
  (->> (map (comp booleong <) measurements (rest measurements))
       (reduce + 0)
       time))

(defn p10
  []
  (->> (map < measurements (rest measurements))
       (reduce (fn [result item]
                 (+ result (booleong item)))
               0)
       time))

(defn p11
  []
  (->> (map < measurements (rest measurements))
       (reduce ((map booleong) +) 0)
       time))

(defn p12
  []
  (->> (map < measurements (rest measurements))
       (transduce (map booleong) + 0)
       time))
