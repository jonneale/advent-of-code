(ns advent-of-code.20)

(defn factors
  [x]
  (set (conj (filter #(zero? (mod x %)) (range 1 (inc (/ x 2))))
             x)))

(defn prime?
  [x]
  (or (and (odd? x)
           (nil? (some #(zero? (mod x %)) (range 3 (inc (Math/sqrt x)) 2))))
      (= 2 x)))

(defn prime-factors
  [x]
  (filter #(and (prime? %)
                (zero? (mod x %))) (range 2 (inc (Math/sqrt x)))))

(defn prime-factorisation
  [x]
  (let [factors (prime-factors x)]
    (for [x ])
    (partition (count factors)
               (for [factor factors
                     pow    (range 1 (inc (count factors)))]
                 [(Math/pow factor pow) pow]))))

(defn total-presents
  [x]
  (reduce + (map (partial * 10) (factors x))))

(defn x
  [sum-of-factors]
  )

(defn run
  [target]
  (let [n (/ target 10)]
    (first (drop-while #(not= target (total-presents %)) (range 1 n)))))


(def x
  (for [x (range 1000)
        y (range 1000)]
    {[x y] :foo}))

(def new-x
  (apply merge (for [x (range 20 50)
                     y (range 10 30)]
                 {[x y] (char x)})))

;;;;;;;;;;;; Solution here, everything above is WIP ;;;;;;;;;;;;;;;;;;;;;;;;;

(defn factors [n]
  (into (sorted-set)
    (mapcat (fn [x] [x (/ n x)])
            (filter #(zero? (rem n %)) (range 1 (inc (Math/sqrt n)))))))


(defn print-factors
  [x target]
  (loop [y x]
    (let [r (reduce + (map #(* % 10) (factors y)))]
      (if (> r target)
        (println "SUCCESS - " y " " r)
        (recur (inc y))))))

(defn filter-elves-who-have-visited-50-houses
  [y factors]
  (drop-while #(>= (/ y %) 50) (sort factors)))

(defn find-part-2
  [x target]
  (loop [y x]
    (let [r (reduce + (map #(* % 11) (filter-elves-who-have-visited-50-houses y (factors y))))]
      (if (> r target)
        (println "SUCCESS - " y " " r)
        (recur (inc y))))))
