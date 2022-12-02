(ns advent-of-code.2020.19
  (:require [clojure.string :as s]))

(def test-rules
  "0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: \"a\"
5: \"b\"")

(def t1
  "0: 1 2
1: \"a\"
2: 1 3 | 3 1
3: \"b\"")

(def t2
  "0: 1 2
1: \"a\"
2: \"b\"")

(def t3
  "0: 1 2
1: \"a\"
2: 1 1 | 3
3: \"b\"")

(def pipe (read-string "|"))

(defn rules->map
  [r]
  (apply merge
         (map #(let [[k v] (s/split % #": ")]
                 {(read-string k) (parse-rule v)}) r)))

(defn parse-rules
  [rules]
  (rules->map (s/split rules #"\n")))

(defn generate-options
  [rule]
  (let [[first-option & remaining-options-with-pipe] (partition-by #(not= pipe %) rule)]
    (concat [first-option] (remove #(= % [pipe]) remaining-options-with-pipe))))

(defn xu
  [rules rule]
  (if (string? rule)
    rule
    (let [rule-values (rules rule)
          conditional-options (generate-options rule-values)]
      (for [option conditional-options]
        (mapcat #(xu rules %) option)))))




"aa | ab"

(defn combine
  [rules rule]
  (let [r (first (xu rules rule))]
    (reduce (fn [agg rule]
              (if (every? char? rule)
                (str agg (apply str rule))
                agg))
            "" r)))




;; 4 1 5






;; ------------------------------------
(defn expand-rule
  [rules rule-index]
  (for [rule (rules rule-index)]
    (if (or (string? rule)
            (symbol? rule))
      rule
      (expand-rule rules rule))))

(defn combine-rules
  [nested-rules agg]
  (if (seq? nested-rules)
    (let [[h & t] nested-rules
          new-agg (str agg (apply str h))]
      (combine-rules t new-agg))
    (str agg nested-rules)))

(defn expand-rule-r
  [rules rule-index & [so-far]]
  (reduce (fn [agg rule]
            (cond (string? rule)
                  (str agg rule)
                  (symbol? rule)
                  (str so-far agg rule)
                  :else (str agg (expand-rule-r rules rule agg))))
          ""
          (rules rule-index)))

(defn flatten-rules
  [agg rule]
  (cond (string? rule)
        (str agg rule)
        (symbol? rule)
        (str agg rule)
        :else
        (let [[option-1 option-2] (s/split (apply str (flatten rule)) #"\|")]
          (if (and option-1 option-2)
            (str agg option-1 "|" agg option-2)
            (str agg option-1)))))

(defn full-exp
  [rules r-i]
  (let [rule (rules r-i)]))

(defn parse-rule
  [rule]
  (map read-string (s/split rule #" ")))
