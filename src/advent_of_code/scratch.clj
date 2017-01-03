
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
