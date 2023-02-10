(ns advent-of-code.2022.14-animate
  (:require [advent-of-code.2022.14 :as core])
  (:import  [javax.swing JFrame JPanel]
            [java.awt Color Graphics Graphics2D]))

;;quil incompatible with jdk9
(def grid-size 600)

(def state (atom (core/parse-input core/input)))


(defn draw-tree [#^Graphics g2d state & [start-x end-x start-y end-y]]
  (doseq [y (range (or start-y 0)  (or end-y (inc grid-size)))]
    (doseq [x (range (or start-x 0) (or end-x (inc grid-size)))]
      (let [e (state [x y])
            v (cond (= e 1) "#" (= e 2) "o" :else ".")]
        (when (= e 1)
          (.drawOval g2d x y (inc x) (inc y)))))))


(defn render [ #^Graphics g w h ]
  (doto g
    (.setColor (Color/BLACK))
    (.fillRect 0 0 w h)
    (.setColor (Color/GREEN)))
  (let [init-length ( / (min w h) 5),
        branch-angle (* 10 (/ w h)),
        max-depth 12]
    (#'draw-tree g @state)))

(defn create-panel []
  "Create a panel with a customised render"
  (proxy [JPanel] []
    (paintComponent [g]
      (proxy-super paintComponent g)
      (time (render g (. this getWidth) (. this getHeight))))))

(def frame
  (JFrame. "Advent of Code day 14"))

(defn run []
  (let [panel (create-panel)]
    (doto frame
      (.add panel)
      (.setSize 800 800)
      (.setVisible true))))
