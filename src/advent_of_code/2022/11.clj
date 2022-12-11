(ns advent-of-code.2022.11
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def ops
  {"*" *'
   "+" +'
   "-" -'
   "/" /})

(defn parse-op
  [operation]
  (let [[_ _ _ f factor] (s/split operation #" ")]
    (fn [x] ((ops f) x
             (if (= "old" factor) x (read-string factor))))))

(defn parse-test
  [test-line true-line false-line]
  (let [factor (read-string (last (s/split test-line #" ")))
        true-result (read-string (last (s/split true-line #" ")))
        false-result (read-string (last (s/split false-line #" ")))]
    (fn [x] (if (zero? (mod x factor))
              true-result
              false-result))))

(defn parse-worry-reduction
  [test-line]
  (let [factor (read-string (last (s/split test-line #" ")))]
    (fn [x] (if (zero? (rem x factor))
              #_factor
              x
              x))))

(defn parse-monkey
  [part-1? monkey]
  (let [[name-line items-line op-line test-line true-line false-line] (s/split monkey #"\n")
        id  (read-string (apply str (butlast (second (s/split name-line #" ")))))
        items (map read-string (s/split (second (s/split items-line #": ")) #", "))
        op    (parse-op (second (s/split op-line #": ")))
        test  (parse-test test-line true-line false-line)
        worry-reduction (parse-worry-reduction test-line)]
    {id {:items items
         :op op
         :test test
         :items-seen 0
         :worry-reduction-level (if part-1? #(/ % 3) worry-reduction)}}))

(defn input
  [part-1?]
  (->> (-> (io/resource "2022/11.txt")
           slurp
           (clojure.string/split #"\n\n"))
       (map (partial parse-monkey part-1?))
       (apply merge)))

(defn test-input
  [part-1?]
  (->> (-> (io/resource "2022/11-test.txt")
           slurp
           (clojure.string/split #"\n\n"))
       (map (partial parse-monkey part-1?))
       (apply merge)))

(defn process-monkey-turn
  [{:keys [items op id test items-seen worry-reduction-level] :as monkey}]
  [(->
    monkey
    (update-in [:items-seen] #(+ % (count items)))
    (assoc :items []))
   (for [item items]
     (let [new-worry (bigint (Math/floor (worry-reduction-level (op item))))]
       [new-worry (test new-worry)]))])

(defn throw-item
  [agg-state [new-worry new-monkey]]
  (update-in agg-state [new-monkey :items] #(conj (vec %) new-worry)))

(defn process-monkey
  [state current-monkey-key]
  (let [[this-monkey updated-items] (process-monkey-turn (state current-monkey-key))
        new-state (assoc state current-monkey-key this-monkey)]
    (reduce throw-item new-state updated-items)))

(defn process-round
  [monkeys]
  (reduce process-monkey monkeys (keys monkeys)))

(defn process-rounds
  [rounds input]
  (reduce (fn[agg _] (process-round agg))
          input
          (range rounds)))

(defn solve-part-1
  []
  (reduce * (take 2 (reverse (sort (map :items-seen (vals (process-rounds 20 (input true)))))))))

(defn solve-part-2
  []
  (take 4 (map :items-seen (vals (process-rounds 45 (test-input false))))))
