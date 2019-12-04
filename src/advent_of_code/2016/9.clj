(ns advent-of-code.2016.9)

(def input
  (clojure.string/replace (clojure.string/trim (slurp "resources/2016/9.txt"))
                          #"\n" ""))

(defn find-compression
  []
  (map last
       (re-seq #"[^\)]\(([^\)]+)" input)))

(defn expand-compression
  [compression]
  (let [compressed-coefficients (clojure.string/split compression #"x")]
    (reduce + (map read-string compressed-coefficients))))

;;--------------------
;; part 2



;;;;;;;;;;;;;;;;;;;;;;;
; Everything in this section was a naive attempt to expand the entire string rather than recursive;ly looking for constants to then expand. It does work, but likely takes hours to run
(defn process-command-string
  [command-string]
  (map #(Integer/parseInt %) (clojure.string/split command-string #"x")))

(defn process-string
  "works in-line but is extraordinarily slow!"
  [s]
  (loop [remaining-string s count-so-far 0 i 0]
    #_(if (zero? (mod i 100000))
      (println (count remaining-string)
               count-so-far "  :   " (apply str (take 20 remaining-string))))
    (cond (empty? remaining-string)
          count-so-far
          (= \( (first remaining-string))
          (let [new-remaining-string (rest remaining-string)
                command-string (apply str (take-while #(not (= \) %)) new-remaining-string))
                command-string-count (count command-string)
                [take-count repeat-count] (process-command-string command-string)
                string-without-command (apply str (drop (inc command-string-count) new-remaining-string))
                take-string (apply str (take take-count string-without-command))
                repeat-string (apply str (repeat (dec repeat-count) take-string))]
            (recur (str repeat-string string-without-command) count-so-far (inc i)))
          :else
          (recur (rest remaining-string) (inc count-so-far) (inc i)))))

(def previous-timestamp (atom (quot (System/currentTimeMillis) 1000)))

(defn print-output
  [count-so-far i remaining-string]
  (let [n 1000000
        new-time (quot (System/currentTimeMillis) 1000)
        total-time-taken (- new-time @previous-timestamp)]

    (if (zero? (mod i n))
      (do
        (println "Time elapsed: " total-time-taken " seconds")
        (println "count-so-far " count-so-far " remaining string count: "(count remaining-string) "  :   " (apply str (take 20 remaining-string)))))))


(defn count-expanded-output
  [s]
  (loop [[first-char & remaining-string] s count-so-far 0 i 0]
    (print-output count-so-far i remaining-string)
    (cond (nil? first-char)
          count-so-far
          (= \( first-char)
          (let [take-str   (apply str (take-while #(not (= \x %)) remaining-string))
                take-str-count (count take-str)
                repeat-str (apply str (take-while #(not (= \) %)) (drop (inc take-str-count) remaining-string)))
                repeat-str-count (count repeat-str)
                command-string-count (+ take-str-count repeat-str-count 4)
                [take-count repeat-count] (map #(Integer/parseInt %) [take-str repeat-str])
                string-without-command (drop (- command-string-count 2) remaining-string)
                string-to-repeat (apply str (take take-count string-without-command))
                repeat-string (apply str (repeat (dec repeat-count) string-to-repeat))
                new-remaining-string (str repeat-string (apply str string-without-command))]
            (recur new-remaining-string count-so-far (inc i)))
          :else
          (recur remaining-string (inc count-so-far) (inc i)))))

;;;ACTUAL SOLUTION:

(defn decompress
  [s]
  (loop [[first-char & remaining-string] s count-so-far 0 i 0]
    (cond (nil? first-char)
          count-so-far
          (= \( first-char)
          (let [take-str   (apply str (take-while #(not (= \x %)) remaining-string))
                take-str-count (count take-str)
                repeat-str (apply str (take-while #(not (= \) %)) (drop (inc take-str-count) remaining-string)))
                repeat-str-count (count repeat-str)
                command-string-count (+ take-str-count repeat-str-count 4)
                [take-count repeat-count] (map #(Integer/parseInt %) [take-str repeat-str])
                string-without-command (drop (- command-string-count 2) remaining-string)]
            (recur (drop take-count string-without-command) (+ count-so-far (+ (* repeat-count (decompress (apply str (take take-count string-without-command)))))) (inc i)))
          :else
          (recur remaining-string (inc count-so-far) (inc i)))))
