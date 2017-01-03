(ns advent-of-code.2016.8)

(defn width
  [grid]
  (reduce max (map first (keys grid))))

(defn height
  [grid]
  (reduce max (map last  (keys grid))))

(defn to-s
  [grid]
  (doseq [y (range (inc (height grid)))]
    (println
     (apply str (for [x (range (inc (width grid)))]
                  (if (zero? (grid [x y])) " " "*"))))))

(defn initial-grid
  [max-x max-y]
  (apply merge
         (reduce concat
                 (for [y (range max-y)]
                   (for [x (range max-x)]
                     {[x y] 0})))))

(defn paint
  [grid [x y]]
  (assoc grid [x y] 1))

(defn rect
  [grid width height]
  (reduce (partial paint)
          grid
          (for [x (range width) y (range height)] 
            [x y])))

(defn update-row
  [grid y new-row]
  (reduce
   (fn [grid [x value]]
     (assoc grid [x y] value))
   grid
   (map-indexed (fn [ix i] [ix i]) new-row)))

(defn update-column
  [grid x new-column]
  (reduce
   (fn [grid [y value]]
     (assoc grid [x y] value))
   grid
   (map-indexed (fn [ix i] [ix i]) new-column)))

(defn row
  [grid y]
  (for [x (range (inc (width grid)))]
    (grid [x y])))

(defn column
  [grid x]
  (for [y (range (inc (height grid)))]
    (grid [x y])))

(defn rotate
  [grid f limit index length]
  (reverse (take (inc (limit grid)) 
                 (drop length 
                       (cycle (reverse (f grid index)))))))
(defn rotate-row
  [grid y b]
  (let [rotated-row (rotate grid row width y b)]
    (update-row grid y rotated-row)))

(defn rotate-column
  [grid x b]
  (let [rotated-column (rotate grid column height x b)]
    (update-column grid x rotated-column)))

(def input
  (clojure.string/split (slurp "resources/2016/8.txt") #"\n"))

(defmulti dispatch
  (fn [grid [command & _]]
    command))

(defmethod dispatch "rect"
  [grid [_ dimensions]]
  (let [[w h] (map read-string (clojure.string/split dimensions #"x"))]
    (rect grid w h)))

(defmethod dispatch "rotate"
  [grid [_ v amount _ distance]]
  (let [f (if (= v "row") rotate-row rotate-column)
        dimension (read-string (last (clojure.string/split amount #"=")))
        d  (read-string distance)]
    (f grid dimension d)))

(defn parse-command
  [grid command]
  (dispatch grid (clojure.string/split command #" ")))

(defn run
  [grid]
  (reduce parse-command grid input))

;; (reduce + (vals (run (initial-grid 50 6))))
