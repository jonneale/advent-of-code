(ns advent-of-code.18)

(def initial-grid
  (clojure.string/split (slurp "./resources/18.txt") #"\n"))

(def test-grid
  [".#.#.#"
   "...##."
   "#....#"
   "..#..."
   "#.#..#"
   "####.."])

(def test-grid-2
  ["##.#.#"
   "...##."
   "#....#"
   "..#..."
   "#.#..#"
   "####.#"])

(defn grid-size
  [grid]
  (reduce max (map first (keys grid))))

(defn print-grid
  [grid]
  (doseq [y (range (inc (grid-size grid)))]
    (doseq [x (range (inc (grid-size grid)))]
      (let [e (grid [x y])
            v (if (= e 1) "#" ".")]
        (print v)))
    (print "\n")))

(defn corner?
  [coord grid-size]
  (or (= [0 0] coord)
      (= [0 grid-size] coord)
      (= [grid-size grid-size] coord)
      (= [grid-size 0] coord)))

(defn parse-grid
  [initial-grid]
  (apply merge-with merge
         (for [x (range (count initial-grid))
               y (range (count initial-grid))]
           (if (corner? [x y] (dec (count initial-grid)))
             {[x y] 1}
             (let [c (nth (nth initial-grid y) x)
                   value (if (= c \.) 0 1)]
               {[x y] value})))))

(defn count-neighbours
  [x y grid]
  (reduce (fn [agg [dx dy]]
            (let [nx (+ x dx)
                  ny (+ y dy)]
              (+ agg (or (grid [nx ny]) 0))))
          0
          (for [dx (range -1 2)
                dy (range -1 2)
                :when (not (and (zero? dx) (zero? dy)))]
            [dx dy])))

(defn update-grid-square
  [[x y] grid grid-size]
  (if (corner? [x y] grid-size)
    {[x y] 1}
    (let [neighbour-count (count-neighbours x y grid)
          current-state   (grid [x y])]
      {[x y] (cond (= 3 (+ current-state neighbour-count))
                   1
                   (= 3 neighbour-count)
                   1
                   :else
                   0)})))

(defn update-grid
  [grid-size grid]
  (apply merge-with merge
         (for [x (range (inc grid-size))
               y (range (inc grid-size))]
           (update-grid-square [x y] grid grid-size))))

(defn run
  [n-steps grid]
  (let [grid (parse-grid grid)
        grid-size (grid-size grid)]
    (first (drop n-steps (iterate (partial update-grid grid-size) grid)))))

(defn count-lights
  []
  (reduce + (vals (run 100 initial-grid))))

(defn count-test-lights
  []
  (reduce + (vals (run 5 test-grid-2))))
