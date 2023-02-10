(ns advent-of-code.2022.22
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn split-input
  [i]
  (clojure.string/split i #"\n\n"))

(def test-map
  "        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.")

(def test-input
  (str test-map "\n\n" "10R5L5R10L4R5L5"))

(def input
  (-> (io/resource "2022/22.txt")
      slurp))

(defn pretty-print
  [v]
  (get {\space " " \. "." \# "#" nil " "} v (if (keyword? v) (name v) v)))

(defn print-map
  [m]
  (let [keys (map first m)
        max-x (reduce max (map first keys))
        max-y (reduce max (map last keys))]
    (doseq [y (range (inc max-y))]
      (println "")
      (doseq [x (range (inc max-x))]
        (print (pretty-print (m [x y])))))))

(defn parse-map
  [i]
  (apply merge
         (let [rows (s/split i #"\n")]
           (for [y (range (count rows))]
             (apply merge (map-indexed (fn[x value] {[x y] value}) (nth rows y)))))))

(defn parse-commands
  [commands]
  (map (partial apply str) (partition-by #(nil? (re-matches #"[0-9]+" (str %))) commands)))

(defn input->map
  [i]
  (parse-map (first (split-input i))))

(defn parse-input
  [i]
  (let [[map-input command-input] i]))

(defn empty-cells
  [i]
  (map first
       (filter (fn[[_ v]] (= v \.)) i)))

(defn possible-cells
  [i]
  (map first
       (filter (fn[[_ v]] (or (= v \.)
                              (= v \#))) i)))

(defn find-starting-position
  [i]
  (first
   (sort-by (juxt last first)
            (empty-cells i))))

(defn next-square
  [current-position facing]
  (let [adjustment
        (case facing
          :N [0 -1]
          :S [0 1]
          :E [1 0]
          :W [-1 0])]
    (map + adjustment current-position)))

(defn teleport
  [[current-x current-y] facing m]
  [(case facing
     :N [current-x (last (last (sort-by last (filter (fn[[x y]] (= x current-x)) (possible-cells m)))))]
     :S [current-x (last (first (sort-by last (filter (fn[[x y]] (= x current-x)) (possible-cells m)))))]
     :E [(first (first (sort-by first (filter (fn[[x y]] (= y current-y)) (possible-cells m))))) current-y]
     :W [(first (last (sort-by first (filter (fn[[x y]] (= y current-y)) (possible-cells m))))) current-y])
   facing])

(defn sizes
  [m]
  (let [max-x (reduce max (map first (keys m)))
        max-y (reduce max (map last  (keys m)))
        face-height (/ (inc max-y) 4)
        face-width  (/ (inc max-x) 3)]
    [face-height face-width]))

(defn cube-face
  [face-sizes [x y] m]
  (let [[face-height face-width] face-sizes]
    (cond (and (> face-height y)
               (< x (* 2 face-width)))
          1
          (and (> face-height y)
               (>= x (* 2 face-width)))
          2
          (and (>= x face-width)
               (< x (* 2 face-width))
               (< y (* 2 face-height)))
          3
          (and (< x face-width)
               (< y (* 3 face-height)))
          4
          (and (>= x face-width)
               (< y (* 3 face-height)))
          5
          :else
          6)))

(defn calculate-face-entry-point
  [face-sizes from-face to-face starting-x starting-y]
  (let [[face-height face-width] face-sizes]
    (case [from-face to-face]
      [1 3]
      [starting-x (inc starting-y)]
      [1 6]
      [0 (+ (* 3 face-height) (- starting-x face-width))]
      [1 4]
      [0 (dec (+ (* 2 face-height) (- face-height starting-y)))]
      [1 2]
      [(inc starting-x) starting-y]
      [2 6]
      [(- starting-x (* 2 face-width)) (dec (* 4 face-height))]
      [2 3]
      [(dec (* 2 face-width)) (+ face-height (- starting-x (* 2 face-width)))]
      [2 5]
      [(dec (* 2 face-width)) (dec (- (* 3 face-height) starting-y))]
      [2 1]
      [(dec starting-x) starting-y]
      [3 2]
      [(+ (* 2 face-width) (- starting-y face-height)) (dec face-height)]
      [3 4]
      [(- starting-y face-height) (* 2 face-height)]
      [3 1]
      [starting-x (dec starting-y)]
      [3 5]
      [starting-x (inc starting-y)]
      [4 3]
      [face-width (+ face-height starting-x)]
      [4 6]
      [starting-x (inc starting-y)]
      [4 5]
      [(inc starting-x) starting-y]
      [4 1]
      [face-width (dec (- (* 3 face-height) starting-y))]
      [5 3]
      [starting-x (dec starting-y)]
      [5 6]
      [(dec face-width) (+ (* 3 face-height) (- starting-x face-width))]
      [5 4]
      [(dec starting-x) starting-y]
      [5 2]
      [(dec (* 3 face-width)) (dec (- (* 3 face-height) starting-y))]
      [6 5]
      [(+ face-width (- starting-y (* 3 face-height))) (dec (* 3 face-height))]
      [6 1]
      [(+ face-width (- starting-y (* 3 face-height))) 0]
      [6 4]
      [starting-x (dec starting-y)]
      [6 2]
      [(+ (* 2 face-width) starting-x) 0])))

(defn get-new-cube-face-and-facing
  [current-cube-face facing]
  (case [facing current-cube-face]
    [:W 1] [:E 4]
    [:E 1] [:E 2]
    [:S 1] [:S 3]
    [:N 1] [:E 6]
    [:W 2] [:W 1]
    [:E 2] [:W 5]
    [:S 2] [:W 3]
    [:N 2] [:N 6]
    [:W 3] [:S 4]
    [:E 3] [:N 2]
    [:S 3] [:S 5]
    [:N 3] [:N 1]
    [:W 4] [:E 1]
    [:E 4] [:E 5]
    [:S 4] [:S 6]
    [:N 4] [:E 3]
    [:W 5] [:W 4]
    [:E 5] [:W 2]
    [:S 5] [:W 6]
    [:N 5] [:N 3]
    [:W 6] [:S 1]
    [:E 6] [:N 5]
    [:S 6] [:S 2]
    [:N 6] [:N 4]))

(defn cube-teleport
  [face-sizes [current-x current-y] facing m]
  (let [current-cube-face (cube-face face-sizes [current-x current-y] m)
        [new-heading new-cube-face]     (get-new-cube-face-and-facing current-cube-face facing)]
    [(calculate-face-entry-point face-sizes current-cube-face new-cube-face current-x current-y)
     new-heading]))

(defn try-move
  [old-position new-position original-facing possible-facing m face-sizes cube? & [counter]]
  (let [contents-of-next-square (get m new-position)
        teleport-fn   (if cube? (partial cube-teleport face-sizes) teleport)]
    (case contents-of-next-square
      \# [old-position original-facing]
      \. [new-position possible-facing]
      (let [[teleport-position teleport-facing] (teleport-fn old-position original-facing m)]
        (if counter
          (do (println [old-position original-facing])
              (throw (Exception. "Shouldn't be here")))
          (recur old-position teleport-position original-facing teleport-facing m face-sizes cube? "true"))))))

(defn move
  [current-position facing distance m face-sizes cube?]
  (if (= distance 0)
    [current-position facing]
    (let [possible-next-square (next-square current-position facing)
          [new-square new-facing] (try-move current-position possible-next-square facing facing m face-sizes cube?)]
      (if (= current-position new-square)
        [current-position facing]
        (move new-square new-facing (dec distance) m face-sizes cube?)))))

(defn update-facing
  [facing move]
  (if (= move "L")
    (case facing
      :N :W
      :E :N
      :S :E
      :W :S)
    (case facing
      :N :E
      :E :S
      :S :W
      :W :N)))

(defn process-command
  [m face-sizes cube? [current-position facing] command]
  (if (re-matches #"[0-9]+" command)
    (move current-position facing (read-string command) m face-sizes cube?)
    [current-position (update-facing facing command)]))

(defn process-commands
  [i cube?]
  (let [[map-input command-input] (split-input i)
        commands                  (parse-commands command-input)
        m                         (parse-map map-input)
        s                         (sizes m)
        f                         #(process-command m s cube? %1 %2)]
    (reductions f [(find-starting-position m) :E] commands)))

(defn part-1
  [i]
  (let [[[x y] facing] (process-commands i false)]
    (+
     (* 1000 (inc y))
     (* 4    (inc x))
     (case facing
       :E 0
       :S 1
       :W 2
       :N 3))))


(defn part-2
  [i]
  (let [[[x y] facing] (last (butlast (process-commands i true)))]
    [[x y facing]
     (+
      (* 1000 (inc y))
      (* 4    (inc x))
      (case facing
        :E 0
        :S 1
        :W 2
        :N 3))]))


(defn face-tests
  []
  (let [full-map (parse-map (first (split-input input)))
        face-sizes (sizes full-map)
        get-face #(cube-face face-sizes % full-map)
        results [(= 1 (get-face [50 0]))
                 (= 1 (get-face [99 0]))
                 (= 1 (get-face [50 49]))
                 (= 1 (get-face [99 49]))
                 (= 4 (get-face [0 100]))
                 (= 4 (get-face [0 149]))
                 (= 4 (get-face [49 100]))
                 (= 4 (get-face [49 149]))
                 (= 3 (get-face [50 50]))
                 (= 3 (get-face [99 50]))
                 (= 3 (get-face [99 99]))
                 (= 5 (get-face [50 100]))
                 (= 5 (get-face [99 100]))
                 (= 5 (get-face [99 149]))
                 (= 6 (get-face [0 150]))
                 (= 6 (get-face [49 150]))
                 (= 6 (get-face [0 199]))
                 (= 6 (get-face [49 199]))
                 (= 2 (get-face [100 0]))
                 (= 2 (get-face [100 49]))
                 (= 2 (get-face [149 0]))
                 (= 2 (get-face [149 49]))]]
    [(every? true? results) " - " results]))


(defn teleport-tests
  []
  (let [face-sizes [50 50]
        f (partial calculate-face-entry-point face-sizes)]
    ["1"
     (= [0 150] (f 1 6 50 0))
     (= [0 199] (f 1 6 99 0))
     (= [100 0] (f 1 2 99 0))
     (= [100 49] (f 1 2 99 49))
     (= [50 50] (f 1 3 50 49))
     (= [99 50] (f 1 3 99 49))
     (= [0 100] (f 1 4 50 49))
     (= [0 149] (f 1 4 50 0))
     "2"
     (= [0 199] (f 2 6 100 0))
     (= [49 199] (f 2 6 149 0))
     (= [99 100] (f 2 5 149 49))
     (= [99 149] (f 2 5 149 0))
     (= [99 50] (f 2 3 100 49))
     (= [99 99] (f 2 3 149 49))
     (= [99 0] (f 2 1 100 0))
     (= [99 49] (f 2 1 100 49))
     "3"
     (= [50 49] (f 3 1 50 50))
     (= [99 49] (f 3 1 99 50))
     (= [100 49] (f 3 2 99 50))
     (= [149 49] (f 3 2 99 99))
     (= [50 100] (f 3 5 50 99))
     (= [99 100] (f 3 5 99 99))
     (= [0 100] (f 3 4 50 50))
     (= [49 100] (f 3 4 50 99))
     "4"
     (= [50 50] (f 4 3 0 100))
     (= [50 99] (f 4 3 49 100))
     (= [50 100] (f 4 5 49 100))
     (= [50 149] (f 4 5 49 149))
     (= [0 150] (f 4 6 0 149))
     (= [49 150] (f 4 6 49 149))
     (= [50 49] (f 4 1 0 100))
     (= [50 0] (f 4 1 0 149))
     "5"
     (= [50 99] (f 5 3 50 100))
     (= [99 99] (f 5 3 99 100))
     (= [149 49] (f 5 2 99 100))
     (= [149 0] (f 5 2 99 149))
     (= [49 150] (f 5 6 50 149))
     (= [49 199] (f 5 6 99 149))
     (= [49 100] (f 5 4 50 100))
     (= [49 149] (f 5 4 50 149))
     "6"
     (= [0 149] (f 6 4 0 150))
     (= [49 149] (f 6 4 49 150))
     (= [50 149] (f 6 5 49 150))
     (= [99 149] (f 6 5 49 199))
     (= [100 0] (f 6 2 0 199))
     (= [149 0] (f 6 2 49 199))
     (= [50 0] (f 6 1 0 150))
     (= [99 0] (f 6 1 0 199))]))
