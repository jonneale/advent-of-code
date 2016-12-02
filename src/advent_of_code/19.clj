(ns advent-of-code.19)

;; (def test-input
;;   (clojure.string/split "H => HO
;; H => OH
;; O => HH" #"\n"))

;; (def test-str "HOH")

;; (defn parse-input
;;   [input]
;;   (map
;;    #(let [[from _ to] (clojure.string/split % #" ")]
;;       [from to])
;;    input))

;; (def input
;;   (clojure.string/split (slurp "./resources/19.txt") #"\n"))

;; (def parsed-input
;;   (parse-input input))

;; (def all-possible-replacements
;;   (into #{} (map first parsed-input)))

;; (def start "e")

;; (defn matcher
;;   [s] 
;;   (re-matcher #"[A-Z]" s))

;; (defn split-string
;;   [s]
;;   (map first (re-seq #"([A-Z][a-z])|([A-Z])" x))

;; (defn find-replacements
;;   [s possible-replacements]
;;   )

;; (defn work
;;   [s possible-replacements]
;;   (for [x (find-replacements 
;;            (split-string s) 
;;            possible-replacements)]
;;     )))





(def s "ORnPBPMgArCaCaCaSiThCaCaSiThCaCaPBSiRnFArRnFArCaCaSiThCaCaSiThCaCaCaCaCaCaSiRnFYFArSiRnMgArCaSiRnPTiTiBFYPBFArSiRnCaSiRnTiRnFArSiAlArPTiBPTiRnCaSiAlArCaPTiTiBPMgYFArPTiRnFArSiRnCaCaFArRnCaFArCaSiRnSiRnMgArFYCaSiRnMgArCaCaSiThPRnFArPBCaSiRnMgArCaCaSiThCaSiRnTiMgArFArSiThSiThCaCaSiRnMgArCaCaSiRnFArTiBPTiRnCaSiAlArCaPTiRnFArPBPBCaCaSiThCaPBSiThPRnFArSiThCaSiThCaSiThCaPTiBSiRnFYFArCaCaPRnFArPBCaCaPBSiRnTiRnFArCaPRnFArSiRnCaCaCaSiThCaRnCaFArYCaSiRnFArBCaCaCaSiThFArPBFArCaSiRnFArRnCaCaCaFArSiRnFArTiRnPMgArF")

(def terminals
  (reduce 
   (fn [agg term]
     (map #(str % term) 
          (flatten (map #(clojure.string/split % (re-pattern term)) agg))))
   [s]
   ["Ar" "Y" "Rn"]))

(defn count-patterns
  [patterns s]
  (count (mapcat #(re-seq % s) patterns)))

(defn count-elements
  [s]
  (let [total-elements (reduce + (map count (clojure.string/split s #"[a-z]")))
        bracket-count (count-patterns [#"Ar" #"Rn"] s)
        comma-count   (count-patterns [#"Y"] s)]
    (- total-elements bracket-count (* 2 comma-count) 1)
    ))
