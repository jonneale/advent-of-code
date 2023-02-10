(ns advent-of-code.2022.20
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def input
  (vec (map read-string
            (->> (-> (io/resource "2022/20.txt")
                     slurp
                     (clojure.string/split  #"\n"))))))

(def test-input [1 2 -3 3 -2 0 4])

(def test-input-2 [2 2 -3 3 -2 0 4])

(def test-input-3 [0 0 8 1])

(def breaking-test-input [1 5 -2 0])

(def test-case
  [0 0 1]);; -> [0 1 0]

(defn preprocess-numbers
  [input]
  (vec
   (map #(vector (java.util.UUID/randomUUID) %) input)))

(defn build-linked-list
  [input]
  (last
   (reduce
    (fn[[previous-node-id agg] [uuid value]]
      (let [new-list (assoc agg uuid
                            {:value value
                             :previous-node-id previous-node-id})
            list-with-previous-updated (if previous-node-id (assoc-in new-list [previous-node-id :next-node-id] uuid) new-list)]
        [uuid list-with-previous-updated]))
    [nil {}]
    input)))

(defn build-list
  [processed-input]
  (let [linked-list (build-linked-list processed-input)
        first-node-id  (first (first (filter #(nil? (:previous-node-id (last %))) linked-list)))
        last-node-id   (first (first (filter #(nil? (:next-node-id (last %))) linked-list)))]
    (-> linked-list
        (assoc-in [first-node-id :previous-node-id] last-node-id)
        (assoc-in [last-node-id  :next-node-id] first-node-id))))

(defn mod-value
  [value linked-list]
  (let [list-count (count (keys linked-list))
        sign       (if (> value 0) + -)]
    (sign (mod (Math/abs value) list-count))))

(defn find-adjacent-node
  [this-previous-node-id this-node-id this-next-node-id linked-list modded-value]
  (cond (= 0 modded-value) this-node-id
        (< 0 modded-value) (let [{:keys [next-node-id previous-node-id value]} (get linked-list this-next-node-id)]
                             (recur this-next-node-id this-next-node-id next-node-id linked-list (dec modded-value)))
        :else              (let [{:keys [next-node-id previous-node-id value]} (get linked-list this-previous-node-id)]
                             (recur previous-node-id this-previous-node-id this-node-id linked-list (inc modded-value)))))

(defn move-right
  [linked-list adjacent-node-id [uuid value]]
  ;; assuming original a->b->c->d
  ;; d and c swap - c is node to move, d is adjacent node
  ;; new order a->b->d->c
  (let [node-to-move                   (get linked-list uuid)
        adjacent-node                  (get linked-list adjacent-node-id)
        adjacent-node-next-node-id     (:next-node-id adjacent-node)
        adjacent-node-previous-node-id (:previous-node-id adjacent-node)]
    (-> linked-list
        ;; b->d
        (assoc-in [(:previous-node-id node-to-move) :next-node-id] (:next-node-id node-to-move))
        ;; b<-d
        (assoc-in [(:next-node-id node-to-move) :previous-node-id] (:previous-node-id node-to-move))
        ;; d->c
        (assoc-in [adjacent-node-id :next-node-id] uuid)
        ;; c<-d
        (assoc-in [uuid :previous-node-id] adjacent-node-id)
        ;; c->a
        (assoc-in [uuid :next-node-id] adjacent-node-next-node-id)
        ;; c<-a
        (assoc-in [adjacent-node-next-node-id :previous-node-id] uuid))))


(defn move-left
  [linked-list adjacent-node-id [uuid value]]
  ;; assuming original a->b->c->d
  ;; c and b swap - c is node to move, b is adjacent node
  ;; new order a->c->b->d
  (let [node-to-move                   (get linked-list uuid)
        adjacent-node                  (get linked-list adjacent-node-id)
        adjacent-node-next-node-id     (:next-node-id adjacent-node)
        adjacent-node-previous-node-id (:previous-node-id adjacent-node)]
    (-> linked-list
        ;; b->d
        (assoc-in [(:previous-node-id node-to-move) :next-node-id] (:next-node-id node-to-move))
        ;; b<-d
        (assoc-in [(:next-node-id node-to-move) :previous-node-id] (:previous-node-id node-to-move))
        ;; a->c
        (assoc-in [adjacent-node-previous-node-id :next-node-id] uuid)
        ;; a<-c
        (assoc-in [uuid :previous-node-id] adjacent-node-previous-node-id)
        ;; c->b
        (assoc-in [uuid :next-node-id] adjacent-node-id)
        ;; c<-b
        (assoc-in [adjacent-node-id :previous-node-id] uuid))))

(defn update-list-elements
  [linked-list adjacent-node-id [uuid value]]
  (cond (= value 0)
        linked-list
        (> value 0)
        (move-right linked-list adjacent-node-id [uuid value])
        :else
        (move-left linked-list adjacent-node-id [uuid value])))

(defn find-element-at-offset
  [uuid offset linked-list]
  (let [{:keys [next-node-id previous-node-id value]} (get linked-list uuid)
        modded-value                                  (mod-value offset linked-list)]
    (find-adjacent-node previous-node-id uuid next-node-id linked-list modded-value)))

(defn update-list
  [linked-list [uuid value]]
  (let [adjacent-node (find-element-at-offset uuid value linked-list)]
    (update-list-elements linked-list adjacent-node [uuid value])))

(defn decrypt
  [input]
  (let [processed-input (preprocess-numbers input)
        linked-list (build-list processed-input)]
    (reduce update-list linked-list processed-input)))


(defn to-list
  [l]
  (let [[first-id v] (first l)]
    (loop [id (v :next-node-id) values [(v :value)]]
      (if (= id first-id)
        values
        (let [{:keys [value next-node-id]} (get l id)]
          (recur next-node-id (conj values value)))))))

(defn solve-part-1
  [i]
  (let [v (decrypt i)
        element-with-value-0-id (first (first (filter #(= 0 (:value (last %))) v)))
        v1-id (find-element-at-offset element-with-value-0-id 1000 v)
        v2-id (find-element-at-offset element-with-value-0-id 2000 v)
        v3-id (find-element-at-offset element-with-value-0-id 3000 v)]
    (reduce
     (fn [agg id]
       (+ agg (get-in v [id :value])))
     0 [v1-id v2-id v3-id])))
