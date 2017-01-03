(ns advent-of-code.2016.2)

(def parse-input
  (clojure.string/split (slurp "resources/2016-2.txt") #"\n"))

(def move
  {\L [-1 0]
   \R [1 0]
   \U [0 -1]
   \D [0 1]})

(defn in-bounds
  [keypad-layout location]
  (let [[x y] location]))

(defn to-key
  [keypad-layout [x y]]
  (nth (nth keypad-layout y) x))

(defn in-bounds?
  [[x y] keypad-layout]
  (and (>= x 0)
       (>= y 0)
       (< y (count keypad-layout))
       (< x (count (nth keypad-layout y)))
       (not= \X (to-key keypad-layout [x y]))))

(defn move-to-new-key
  [keypad-layout [start-x start-y] instruction]
  (let [[d-x d-y]    (move instruction)
        new-location [(+ start-x d-x) (+ start-y d-y)]]
    new-location
    (if (in-bounds? new-location keypad-layout)
      new-location
      [start-x start-y])))

(defn follow-instruction
  [keypad-layout starting-position instruction]
  (reduce (partial move-to-new-key keypad-layout) starting-position instruction))


(defn generate-key-locations
  [keypad-layout instructions starting-position]
  (reductions (partial follow-instruction keypad-layout) starting-position instructions))

(defn find-code
  [instructions keypad-layout starting-position]
  (map (partial to-key keypad-layout) (drop 1 (generate-key-locations keypad-layout instructions starting-position))))

(def standard-layout
  [[1 2 3] [4 5 6] [7 8 9]])

[[\X \X 1 \X \X] [\X 2 3 4 \X] [5 6 7 8 9] [\X \A \B \C \X] [\X \X \D \X \X]]

#_ (find-code parse-input standard-layout [1 1])

#_ (find-code parse-input fancy-layout [0 2])

