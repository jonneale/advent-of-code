(ns advent-of-code.2020.15)

(def i1 [0,3,6])
(def i2 [0,13,16,17,1,10,6])

(defn run-game
  ([number-list max]
   (run-game (last number-list) (apply merge (map-indexed (fn[ix n] {n [ix]}) number-list)) (count number-list) max))
  ([previous-number history current-turn max]
   (if (>= current-turn max)
     previous-number
     (let [spoken-number (if (= [(dec current-turn)] (history previous-number))
                           0
                           (apply - (take 2 (reverse (sort (history previous-number))))))
           updated-history (update history spoken-number #(if % (conj [(last (sort %))] current-turn) [current-turn]))]
       (recur spoken-number updated-history (inc current-turn) max)))))




(defn run-game
  ([number-list max]
   (run-game (last number-list) (apply merge (map-indexed (fn[ix n] {n ix}) number-list)) (count number-list) true max))
  ([previous-number history current-turn first-time? max]
   (if (>= current-turn max)
     previous-number
     (if first-time?
       (recur 0 (assoc history previous-number (dec current-turn)) (inc current-turn) (nil? (history 0)) max)
       (let [spoken-number  (- (dec current-turn) (history previous-number))]
         (recur spoken-number (assoc history previous-number (dec current-turn)) (inc current-turn) (nil? (history spoken-number)) max))))))
