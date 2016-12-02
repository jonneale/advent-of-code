(ns advent-of-code.2016.1)

(def parse-input
  (map #(clojure.string/replace % #"[^A-Z0-9]" "") (clojure.string/split (slurp "resources/2016-1.txt") #",")))

(def state-machine
  {[\N \R] \E
   [\N \L] \W
   [\E \R] \S
   [\E \L] \N
   [\S \R] \W
   [\S \L] \E
   [\W \R] \N
   [\W \L] \S})

(defmulti move
  (fn [direction]
    direction))

(defmethod move \N
  [_]
  [0 1])

(defmethod move \S
  [_]
  [0 -1])

(defmethod move \W
  [_]
  [-1 0])

(defmethod move \E
  [_]
  [1 0])

(defn map-input-to-transition
  [[current-direction transitions] [direction & distance]]
  (let [new-direction (state-machine [current-direction direction])
        int-distance  (read-string (apply str distance))]
    [new-direction (into transitions (repeat int-distance (move new-direction)))]))

(defn generate-route
  [inputs]
  (reduce map-input-to-transition [\N []] inputs))

(defn follow-route
  [[old-x old-y] [d-x d-y]]
  [(+ old-x d-x) (+ old-y d-y)])

(defn all-interim-positions
  [inputs]
  (reductions follow-route [0 0] (last (generate-route inputs))))

(defn distance-to-position
  [position]
  (reduce + (map #(Math/abs %) position)))

(defn distance-to-final-position
  [inputs]
  (distance-to-position (last (all-interim-positions inputs))))

(defn first-position-visited-twice
  [inputs]
  (let [numbered-positions (map-indexed #(cons %1 %2) (all-interim-positions inputs))]
    (->> numbered-positions 
         (group-by (fn [[_ x y]] [x y]))
         (filter #(> (count (last %)) 1))
         (map (fn [[position occurences]] [position (reduce max (map first occurences))]))
         (sort-by last)
         first
         first
         distance-to-position)))
