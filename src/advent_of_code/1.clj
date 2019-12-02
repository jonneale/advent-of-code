(ns advent-of-code.1)

(def input (clojure.string/trim-newline (slurp "resources/1.txt")))

(def mask
  {\( 1
   \) -1})

(defn- all-floors
  [input]
  (reductions + (map mask input)))

(defn which-floor
  [input]
  (last (all-floors input)))

(defn first-basement
  [input]
  (inc (count (take-while (comp not neg?) (all-floors input)))))


;;
    ;;    1
    ;;  2   2
    ;; 3 4 4 3
(def t1
  [1 [2 [3 nil nil] [4 nil nil]] [2 [4 nil nil] [3 nil nil]]])


(defn balanced-tree?
  [t]
  (loop [collected-branches [t]]
    (let [values-at-level (map first collected-branches)
          branches (concat (map second collected-branches)
                           (map last collected-branches))]
      (if-not (= values-at-level (reverse values-at-level))
        false
        (if-not (some sequential? branches)
          true
          (recur branches))))))

(defn balanced?
  ;; all branches
  [tree]
  (loop [t tree level 0 value-map {}]
    (let [[v l r] t]
      
      (if (sequential? l)
        (recur l (inc level) (assoc value-map level v))
        (assoc value-map level v)))))


(defn nested-sum-less-than
  [max-val]
  (fn [[sum agg-list] [h t]]
    (if (> max-val (+ sum h))
      agg-list
      ())))



(fn horriblis
  ([max-value sum agg-list [h & t]]
   (if h
     (if (integer? h)
       (let [new-sum (+ sum h)]
         (if (> new-sum max-value)
           [sum agg-list]
           (recur max-value new-sum (conj agg-list h) t)))
       (let [[new-sum sub-agg-list] (horriblis max-value sum [] h)]
         (recur max-value new-sum (conj agg-list sub-agg-list) t)))
     [sum agg-list]))
  ([max-value l]
   (last (horriblis max-value 0 [] l))))


(defn t
  [f]
  (nth (filter f (range)) 16))



(defn balanced-prime?
  [x]
  (letfn [(is-prime? [x]
            (and (> x 2)
                 (empty? (filter #(zero? (rem x %)) 
                                 (range 2 (inc (Math/pow x 0.5)))))))
          (next-prime [x int-range]
            (first (drop-while #(not (is-prime? %)) int-range)))]
    (and (is-prime? x)
         (when-let [previous-prime (next-prime x (range (dec x) -1 -1))]
           (= (double x) 
              (/ (+ previous-prime
                    (or (next-prime x (range (inc x) 
                                             (inc (+ x (- x previous-prime))))) 0))
                 2.0))))))

(defn render-rotated-square
  [side-length]
  (repeat side-length 
          (apply str (conj (repeat side-length "*")))))

(defn pp-squares
  [start end]
  (let [squares     (take-while #(< % (inc end)) (iterate #(* % %) start))
        digit-count (count (apply str squares))]
    digit-count))
