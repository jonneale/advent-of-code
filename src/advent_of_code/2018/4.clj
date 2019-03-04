(ns advent-of-code.2018.4
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn to-int
  [s]
  (Integer/parseInt s))

(defn to-timestamp
  [log-entry]
  (let [date-string (apply str (rest (take 17 log-entry)))
        [date-str time-str] (s/split date-string #" ")
        [year month day]    (map to-int (s/split date-str #"-"))
        [hours minutes]     (map to-int (s/split time-str #":"))]
    [year month day hours minutes]))

(defn parse-input
  []
  (sort-by to-timestamp (s/split (slurp (io/resource "2018/4.txt")) #"\n")))

(defmulti handle-entry
  (fn [command entry whole-entry guard-data]
    command))

(defmethod handle-entry "Guard"
  [_ entry whole-entry guard-data]
  (assoc guard-data :current-guard (second entry)))

(defmethod handle-entry "falls"
  [_ entry whole-entry guard-data]
  (let [timestamp (to-timestamp whole-entry)
        current-guard (:current-guard guard-data)
        minute (last timestamp)
        year-month-day (take 3 timestamp)
        current-guard-key [current-guard year-month-day]
        data-for-current-guard (get-in guard-data current-guard-key)]
    (if data-for-current-guard
      (update-in guard-data current-guard-key #(conj % minute))
      (assoc-in guard-data current-guard-key [minute]))))

(defmethod handle-entry "wakes"
  [_ entry whole-entry guard-data]
  (let [timestamp (to-timestamp whole-entry)
        current-guard (:current-guard guard-data)
        minute (last timestamp)
        year-month-day (take 3 timestamp)
        current-guard-key [current-guard year-month-day]
        data-for-current-guard (get-in guard-data current-guard-key)]
    (if data-for-current-guard
      (update-in guard-data current-guard-key #(concat % (range (inc (last %)) minute)))
      (assoc-in guard-data current-guard-key (range 0 minute)))))

(defn update-guard-data
  [log-entry guard-data]
  (let [entry     (drop 2 (s/split log-entry #" "))]
    (handle-entry (first entry) entry log-entry guard-data)))

(defn analyse-log
  []
  (loop [input (parse-input) guard-data {}]
    (if (nil? input)
      (dissoc guard-data :current-guard)
      (let [[log-entry & remaining-logs] input
            updated-guard-data (update-guard-data log-entry guard-data)]
        (recur remaining-logs updated-guard-data)))))

(defn count-up-minutes
  [[k vs]]
  (count (reduce concat (vals vs))))

(defn count-asleep-time
  []
  (let [data (analyse-log)]
    (let [[most-asleep how-asleep] (->> data
                                        (map (fn [d] [(first d) (count-up-minutes d)]))
                                        (sort-by last)
                                        last)
          minute-most-often-asleep (first (last (sort-by last (frequencies (reduce concat (vals (data most-asleep)))))))]
      [most-asleep minute-most-often-asleep])))

(defn most-common-asleep-minute
  [d]
  (->> d
       vals
       (reduce concat)
       frequencies
       (sort-by last)
       last))

(defn generate-answer-part-2
  []
  (->> (analyse-log)
       (map (fn [[id data]]
              (into [id] (most-common-asleep-minute data))))
       (sort-by last)
       last))

(defn generate-answer-part-1
  []
  (let [[guard-id minute-asleep] (count-asleep-time)]))
