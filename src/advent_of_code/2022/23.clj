(ns advent-of-code.2022.23
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn get-inf
  [f]
  (let [infs (set [Double/POSITIVE_INFINITY Double/NEGATIVE_INFINITY])]
    (first (clojure.set/difference infs #{(apply f infs)}))))

(defn extreme-values
  [grid f]
  (reduce (fn[[x-max y-max] [x y]]
            [(f x-max x)
             (f y-max y)])
          [(get-inf f)
           (get-inf f)]
          (keys grid)))

(defn print-grid
  [grid]
  (let [[min-x min-y] (extreme-values grid min)
        [max-x max-y] (extreme-values grid max)]
    (doseq [y (range min-y (inc max-y))]
      (doseq [x (range min-x (inc max-x))]
        (let [e (grid [x y])
              v (cond (= e 1) "#" (= e 2) "o" :else ".")]
          (print v)))
      (print "\n"))))

(def t1
  [".............."
   ".............."
   ".......#......"
   ".....###.#...."
   "...#...#.#...."
   "....#...##...."
   "...#.###......"
   "...##.#.##...."
   "....#..#......"
   ".............."
   ".............."
   ".............."])

(def t2
  ["....."
   "..##."
   "..#.."
   "....."
   "..##."
   "....."])

(def input
    (->> (-> (io/resource "2022/23.txt")
              slurp
             (clojure.string/split  #"\n"))))


(defn parse-input
  [i]
  (apply merge
         (for [y (range (count i))
               x (range (count (first i)))]
           {[x y] (if (= \# (nth (nth i y) x)) 1 0)})))


(def moves
  {:N  [0 -1]
   :NE [1 -1]
   :E  [1 0]
   :SE [1 1]
   :S  [0 1]
   :SW [-1 1]
   :W  [-1 0]
   :NW [-1 -1]})

(defn no-neighbours?
  [coords grid directions]
  (= 0
     (reduce (fn[agg direction]
               (+ agg
                  (get grid (map + coords (direction moves)) 0)))
             0
             directions)))

(defn move
  [coords direction]
  (map + coords (moves direction)))

(defn only-uniques
  [l]
  (reduce (fn[agg [key elf-moves]]
            (if (= (count elf-moves) 1)
              (conj agg (first elf-moves))
              agg))
          []
          (group-by second l)))

(defn propose-move
  [coords parsed-input move-proposals]
  (if (no-neighbours? coords parsed-input (keys moves))
    coords
    (reduce (fn[new-coords [directions-to-check move-direction]]
              (if (and (= new-coords coords)
                       (no-neighbours? coords parsed-input directions-to-check))
                (move coords move-direction)
                new-coords))
            coords
            move-proposals)))

(def move-proposals
  [[[:N :NE :NW] :N]
   [[:S :SE :SW] :S]
   [[:W :NW :SW] :W]
   [[:E :NE :SE] :E]])

(defn process-all-elves
  [move-proposals parsed-input]
  (let [[max-x max-y] (extreme-values parsed-input max)
        [min-x min-y] (extreme-values parsed-input min)]
    (for [x (range min-x (inc max-x))
          y (range min-y (inc max-y))
          :when (= 1 (parsed-input [x y]))]
      (conj [[x y]]
            (propose-move [x y] parsed-input move-proposals)))))

(defn move-elves
  [elf-moves]
  (apply merge (map (fn[[from to]]
                     {from 0
                      to 1}) elf-moves)))

(defn remove-non-movers
  [l]
  (remove (fn[[a b]] (= a b)) l))

(defn advance-one-step
  [parsed-input move-proposals]
  (->> parsed-input
       (process-all-elves move-proposals)
       remove-non-movers
       only-uniques
       move-elves
       (merge parsed-input)) )

(defn advance-n-steps
  [parsed-input n]
  (loop [i 0 grid-state parsed-input moves (cycle move-proposals)]
    (if (= i n)
      grid-state
      (recur (inc i) (advance-one-step grid-state (take 4 moves)) (rest moves)))))


(defn part-1
  [i]
  (let [grid-state (advance-n-steps (parse-input i) 10)
        elves      (filter (fn[[coods elf]] (= elf 1)) grid-state)
        [max-x max-y] (extreme-values elves max)
        [min-x min-y] (extreme-values elves min)]
    (reduce (fn [agg [x y]] (+ agg (- 1 (get grid-state [x y] 0)))) 0
            (for [x (range min-x (inc max-x))
                  y (range min-y (inc max-y))]
              [x y]))))

(defn part-2
  [input]
  (loop [i 0 grid-state (parse-input input) moves (cycle move-proposals)]
    (when (zero?(mod i 100)) (println i))
    (let [new-grid-state (advance-one-step grid-state (take 4 moves))]
      (if (= grid-state new-grid-state)
        i
        (recur (inc i) new-grid-state (rest moves))))))
