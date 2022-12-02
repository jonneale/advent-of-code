(ns scratch)

(def options
  ["{ac[bb]}"
   "[dklf(df(kl))d]"
   "{}"
   "{[[[]]]}"
   "{3234[fd"
   "{df][d}"])


(def opening-parens
  #{"[" "(" "{"})

(def closing-parens
  #{"]" ")" "}"})

(def paren-regex
  (re-pattern (str "[^\\" (apply str (interpose "\\" (concat opening-parens closing-parens))) "]")))

(defn closes?
  [opening closing]
  (or (and (= opening "[")
           (= closing "]"))
      (and (= opening "(")
           (= closing ")"))
      (and (= opening "{")
           (= closing "}"))))

(defn check-matching
  [s]
  (let [parens (clojure.string/replace s paren-regex "")]
    (loop [paren-stack [] remaining parens]
      (if (clojure.string/blank? remaining)
        (empty? paren-stack)
        (let [[f-char & r] remaining
              f-str (str f-char)
              rest (apply str r)]
          (if (contains? opening-parens f-str)
            (recur (conj paren-stack f-str) rest)
            (if (closes? (str (last paren-stack)) f-str)
              (recur (butlast paren-stack) rest)
              false)))))))











(def letter-scores
  [[[\D \G]          2]
   [[\B \C \M \P]    3]
   [[\F \H \V \W \Y] 4]
   [[\K]             5]
   [[\J \X]          8]
   [[\Q \Z]          10]])

(def letter-score-map
  (reduce (fn[agg [letters score]]
            (reduce #(assoc %1 %2 score) agg letters))
          {}
          letter-scores))

(defn score-for-letter
  [letter]
  (get letter-score-map letter 1))


(defn score-scrabble
  [word]
  (apply + (map score-for-letter (clojure.string/upper-case word))))

(defmulti apply-effect
  (fn [score effect]
    effect))

(defmethod apply-effect :dls
  [score _]
  [(* score 2) 1])

(defmethod apply-effect :tls
  [score _]
  [(* score 3) 1])

(defmethod apply-effect :dws
  [score _]
  [score 2])

(defmethod apply-effect :tws
  [score _]
  [score 3])

(defmethod apply-effect :default
  [score _]
  [score 1])

(defn score-letter
  [letter square-effect]
  (apply-effect (get letter-score-map letter 1) square-effect))

(defn score-and-multiple
  [word square-effects]
  (map score-letter (clojure.string/upper-case word)
       square-effects))

(defn sum-scores-and-multiples
  [scores-and-multiples]
  (reduce (fn[[score-agg multiple-agg]
              [score multiple]]
            [(+ score-agg score)
             (* multiple-agg multiple)])
          [0 1]
          scores-and-multiples))

(defn aggregate-score-and-multiple
  [word square-effects]
  (sum-scores-and-multiples
   (score-and-multiple word square-effects)))

(defn score-with-square-effects
  [word square-effects]
  (let [[score multiple] (aggregate-score-and-multiple word square-effects)]
    (* score multiple)))

(comment (aggregate-score-and-multiple "cabbage" [:dls nil nil nil nil nil nil])) #_ (17 1)

(comment (aggregate-score-and-multiple "cabbage" [:dls nil nil nil nil nil :dws])) #_ (17 2)

(comment (score-with-square-effects "cabbage" [:dls nil nil nil nil nil :dws])) #_ 34

(comment (aggregate-score-and-multiple "cabbage" [:dls nil nil nil :tws nil :dws])) #_ (17 6)

(comment (score-with-square-effects "cabbage" [:dls nil nil nil :tws nil :dws])) #_ 102
