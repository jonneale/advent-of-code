(ns advent-of-code.3)

(def mask
  {\^ [0 -1]
   \> [1 0]
   \v [0 1]
   \< [-1 0]})

(defn- add-vector
  [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn- track-santa
  [all-positions direction]
  (let [most-recent-position (first all-positions)
        move                 (mask direction)]
    (cons (add-vector move most-recent-position) all-positions)))

(defn- move-santa
  [inputs]
  (let [initial-position [0 0]]
    (reduce track-santa
            [[0 0]]
            inputs)))

(defn count-distinct-positions
  [positions]
  (count (distinct positions)))

(defn count-santa-positions
  [inputs]
  (count-distinct-positions (move-santa inputs)))

(defn count-santa-and-robo-santa-positions
  [inputs]
  (let [santa-directions (take-nth 2 inputs)
        robo-santa-directions (take-nth 2 (rest inputs))]
    (count-distinct-positions
     (concat (move-santa santa-directions)
             (move-santa robo-santa-directions)))))
