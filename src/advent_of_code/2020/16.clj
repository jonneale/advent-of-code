(ns advent-of-code.2020.16
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def i
  "class: 0-1 or 4-19
row: 0-5 or 8-19
seat: 0-13 or 16-19

your ticket:
11,12,13

nearby tickets:
3,9,18
15,1,5
5,14,9")

(def input (slurp (io/resource "2020/16.txt")))

(defn parse-tickets
  [list-of-tickets]
  (map (fn[ticket] (map read-string (s/split ticket #",")))
   (rest (s/split list-of-tickets #"\n"))))

(defn to-rule
  [rule-text]
  (map read-string (s/split rule-text #"-")))

(defn parse-rule
  [rule-string]
  (let [[name rule-body] (s/split rule-string #": ")
        rule-text  (s/split rule-body #" or ")]
    (concat [name] (map to-rule rule-text))))

(defn parse-rules
  [rules]
  (map parse-rule (s/split rules #"\n")))

(defn parse-input
  [i]
  (let [[rules my-ticket other-tickets] (s/split i #"\n\n")]
    [(parse-rules rules) (parse-tickets other-tickets) (first (parse-tickets my-ticket))]))

(defn rule-applies?
  [[name [lb-min lb-max] [up-min up-max]] value]
  (or (and (>= value lb-min)
           (<= value lb-max))
      (and (>= value up-min)
           (<= value up-max))))

(defn valid-for-any-rule?
  [rules value]
  (reduce (fn [agg rule]
            (or agg
                (rule-applies? rule value)))
          false
          rules))

(defn which-rules-apply
  [rules ticket]
  (for [ticket-value ticket]
    (filter #(rule-applies? % ticket-value) rules)))

(defn which-value-is-invalid?
  [rules ticket]
  (for [ticket-value ticket]
    [ticket-value (valid-for-any-rule? rules ticket-value)]))

(defn conforms-to-rules?
  [rules ticket]
  (reduce (fn [agg value] (and agg (valid-for-any-rule? rules value))) true ticket))

(defn count-invalid-tickets
  [i]
  (let [[rules other-tickets my-ticket] (parse-input i)]
    (let [invalid-tickets (remove (partial conforms-to-rules? rules) other-tickets)]
      (reduce + (mapcat (fn[ticket] (map first (filter (comp false? last) (which-value-is-invalid? rules ticket)))) invalid-tickets)))))


(defn determine-rule-positions
  ([rules]
   (determine-rule-positions rules []))
  ([rules known-rule-positions]
   (if (empty? rules)
     known-rule-positions
     (let [[next-rule & remaining-rules]      (sort-by (comp count last) rules)
           next-rule-text                     (first (last next-rule))
           next-rule-position                 (first next-rule)
           updated-other-rules                (for [[other-rule-position other-rule] remaining-rules]
                                                [other-rule-position
                                                 (remove (partial = next-rule-text) other-rule)])]
       (recur updated-other-rules (conj known-rule-positions [next-rule-position next-rule-text]))))))

(defn set-of-rules-for-each-ticket
  "Numbering is horrible hack to save having to re-parse the data and add rules - retain order when sorting/recurring"
  [rules other-tickets my-ticket]
  (let [valid-tickets (filter (partial conforms-to-rules? rules) other-tickets)
        applicable-rules (map (partial which-rules-apply rules) valid-tickets)]
    (for [ticket-value-index (range (count my-ticket))]
      [ticket-value-index (apply clojure.set/intersection (map (comp set #(nth % ticket-value-index)) applicable-rules))])))

(defn solve-part-2
  [rules my-ticket]
  (reduce
   (fn [agg [index [name & rest]]]
     (if (.startsWith name "departure")
       (let [my-ticket-value (nth my-ticket index)]
         (* agg my-ticket-value))
       agg))
   1 rules))

(defn process-tickets
  [i]
  (let [[rules other-tickets my-ticket] (parse-input i)]
    (-> (set-of-rules-for-each-ticket rules other-tickets my-ticket)
        determine-rule-positions
        (solve-part-2 my-ticket))))
