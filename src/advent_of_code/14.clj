(ns advent-of-code.14)

(def input
  (clojure.string/split (slurp "./resources/14.txt") #"\n"))

(def test-input
  ["Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds."
   "Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds."])

(defn parse-input
  [i]
  (let [name          (re-find #"[^\s]+" i)
        speed         (read-string (last (re-find #"([\d]+) km/s" i)))
        duration      (read-string (last (re-find #"([\d]+) seconds" i)))
        rest-duration (read-string (last (re-find #"rest for ([\d]+) seconds" i)))]
    [name speed duration rest-duration]))

(defn generate-seq
  [[speed duration rest-duration]]
  (cycle (concat (repeat duration speed) (repeat rest-duration 0))))

(defn distances-over-time
  [time input]
  (for [i input]
    (let [[name & speed-data] (parse-input i)]
      [name (reduce + (take time (generate-seq speed-data)))])))

(defn longest-distance
  [time input]
  (reverse (sort-by last (distances-over-time time input))))

(defn score-reindeer
  [input time]
  (let [distances (longest-distance time input)
        [[leading-reindeer leading-distance] & _] distances]
    (map first (take-while #(= leading-distance (last %)) distances))))

(defn total-scores
  [time input]
  (frequencies (mapcat (partial score-reindeer input) (range 1 (inc time)))))
