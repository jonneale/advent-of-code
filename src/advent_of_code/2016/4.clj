(ns advent-of-code.2016.4
  (:require [clojure.string :as s]))

(def parse-input
  (for [line (clojure.string/split (slurp "resources/2016-4.txt") #"\n")]
    (partition-by #(nil? (re-find #"[0-9]" (str %))) (s/replace line #"-" ""))))

(defn comparer
  [[char1 frequency1] [char2 frequency2]]
  (or (> frequency1 frequency2)
      (and (= frequency1 frequency2)
           (> (int char2)
              (int char1)))))

(defn matches-checksum?
  [[code room checksum]]
  (let [calculated-checksum (apply str (map first (take 5 (sort comparer (frequencies code)))))
        actual-checksum (apply str (butlast (rest checksum)))]
    (= actual-checksum calculated-checksum)))

(defn find-matches
  [input]
  (filter matches-checksum? input))

(defn to-room-number
  [room]
  (read-string (apply str (second room))))

(defn sum-correct-rooms
  [input]
  (reduce + (map to-room-number (find-matches input))))

(defn decrypt-letter
  [sector-id c]
  (+ 97 (mod (+ sector-id (- (int c) 97)) 26)))

(defn decrypt-room-name
  [room-details]
  (let [[room-name sector-id _] room-details 
        sector-id (to-room-number room-details)]
    [(apply str (map (comp char (partial decrypt-letter sector-id)) room-name)) sector-id]))

(defn decrypt-valid-rooms
  [input]
  (map decrypt-room-name (find-matches input)))

(defn find-north-pole-objects
  [input]
  (last (filter (comp #(= "northpoleobjectstorage" %) first) (decrypt-valid-rooms input))))
