(ns advent-of-code.2016.12)

(def parse-input
  (map #(clojure.string/replace % #"[^A-Z0-9]" "") (clojure.string/split (slurp "resources/2016-1.txt") #",")))

(def state-machine
  {["N" "R"] "E"
   ["N" "L"] "W"
   ["E" "R"] "S"
   ["E" "L"] "N"
   ["S" "R"] "W"
   ["S" "L"] "E"
   ["W" "R"] "N"
   ["W" "L"] "S"})

(defmulti move
  (fn [direction distance x y]
    (str direction)))

(defmethod move "N"
  [direction distance x y]
  [x (+ y distance)])

(defmethod move "S"
  [direction distance x y]
  [x (- y distance)])

(defmethod move "W"
  [direction distance x y]
  [(- x distance) y])

(defmethod move "E"
  [direction distance x y]
  [(+ x distance) y])

(defn follow-input
  [[starting-direction x y] input]
  (let [[direction & distance] input
        movement-direction (state-machine [(str starting-direction) (str direction)])
        [result-x result-y] (move movement-direction (read-string (apply str distance)) x y)]
    [movement-direction result-x result-y]))

(defn follow-route
  [input]
  (reductions follow-input ["N" 0 0] input))

(defn abs
  [x]
  (if (zero? x) x
      (Math/sqrt (* x x))))

(defn find-smallest-second
  [values]
  (reduce (fn [best-so-far points]
            (let [latest-visit (reduce max (map first points))]
              (if (> (first best-so-far) latest-visit)
                (first points)
                best-so-far))) 
          [999999]
          (map last values)))

(defn first-visited-twice
  [input]
  (find-smallest-second
   (filter #(> (count (second %)) 1)
           (group-by (juxt last (comp last butlast)) 
                     (map-indexed (fn [ix v] (cons ix v)) (follow-route input))))))

(defn distance-to-point-visited-twice
  [input]
  (reduce +
          (map abs
               (drop 2
                     (find-smallest-second input)))))

(defn run-to-end
  [input]
  (reduce + (map abs (rest (last (follow-route input))))))
