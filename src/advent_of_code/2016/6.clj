(ns advent-of-code.2016.6)


(defn to-char-positions
  [line]
  (map-indexed hash-map line))

(defn letters-by-position
  [indexed-data]
  (group-by (comp first keys) (reduce concat indexed-data)))

(def input
  (clojure.string/split (slurp "resources/2016-6.txt") #"\n"))

(def test-input
  ["eedadn"
   "drvtee"
   "eandsr"
   "raavrd"
   "atevrs"
   "tsrnev"
   "sdttsa"
   "rasrtv"
   "nssdts"
   "ntnada"
   "svetve"
   "tesnvt"
   "vntsnd"
   "vrdear"
   "dvrsen"
   "enarar"])

(defn parse-input
  [i]
  (letters-by-position (map to-char-positions i)))

(defn most-popular-by-position
  [input]
  (for [[position letter-values] input]
    (let [letters-in-position              (map (comp first vals) letter-values)
          sorted-letters                   (sort-by last (frequencies letters-in-position))
          most-frequent-letter-in-position (last sorted-letters)
          least-frequent-letter-in-position (first sorted-letters)]
      [position 
       (first most-frequent-letter-in-position)
       (first least-frequent-letter-in-position)])))

(defn calculate-message-by-most-frequent
  [input]
  (apply str (map second (most-popular-by-position input))))

(defn calculate-message-by-least-frequent
  [input]
  (apply str (map last (most-popular-by-position input))))
