(ns user)

(def s "A(1x5)BC")
(def s2 "A(2x2)BCD(2x2)EFG")
(def s3 "(6x1)(1x3)A")
(def s4 "X(8x2)(3x3)ABCY")
(def input (slurp "resources/2017/9.txt"))

(defn parse-repeat
  [c]
  (let [input (apply str c)
        s (second (clojure.string/split input #"[\(\)]"))
        repeat-data (clojure.string/split s #"x")]
    [(map #(Integer/parseInt %) repeat-data) (apply str (rest (drop-while #(not= % \)) input)))]))

(defn decompress
  [input]
  (loop [in input out "" characters-left-to-repeat 0 repeat-times 0 repeat-str ""]
    (cond (and (empty? repeat-str) (empty? in))
          out
          (> characters-left-to-repeat 0)
          (recur (rest in) out (dec characters-left-to-repeat) repeat-times (str repeat-str (first in)))
          (not (empty? repeat-str))
          (recur in 
                 (str out (apply str (repeat repeat-times repeat-str)))
                 0
                 0
                 "")
          (= \( (first in))
          (let [[[number-of-characters-to-repeat times-to-repeat] remaining-in] (parse-repeat in)]
            (recur remaining-in out number-of-characters-to-repeat times-to-repeat ""))
          :else
          (recur (rest in) (str out (first in)) characters-left-to-repeat repeat-times repeat-str))))

(defn count-decompressed
  [s]
  (count (remove #(re-matches #"[^A-Zx0-9\(\)]" (str %)) (decompress (reverse s)))))
