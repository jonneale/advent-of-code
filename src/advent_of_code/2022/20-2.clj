(ns advent-of-code.2022.20-2
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn abs
  [x]
  (if (> 0 x) (- x) x))

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
  [0 0 1])


(Math/abs 200);; => 200
(Math/abs -200);; => 200
(Math/abs 3246356612)
(Math/abs -11110000054545200)

(Math/abs 200)   ;; => 200
(Math/abs -200)  ;; => 200
(Math/abs (first [200])) ;; => 200(Math/abs 4057945765)    ;; => 4057945765
(Math/abs (first [4057945765]))   ;; => 237021531


(defn preprocess-numbers
  [input decryption-key]
  (vec
   (map #(vector (java.util.UUID/randomUUID) (* (or decryption-key 1) %)) input)))

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

(defn mod-value
  [value linked-list & [simple-mod?]]
  (if simple-mod? value
      (let [list-count (dec (count (keys linked-list)))
            sign       (if (> value 0) + -)
            move-length (mod (abs value) list-count)
            move-value (sign move-length)
            opposite-move-value (- move-value (sign list-count))]
        (if (> move-length (abs opposite-move-value))
          opposite-move-value
          move-value))))

(defn build-list
  [processed-input]
  (let [linked-list (build-linked-list processed-input)
        first-node-id  (first (first (filter #(nil? (:previous-node-id (last %))) linked-list)))
        last-node-id   (first (first (filter #(nil? (:next-node-id (last %))) linked-list)))]
    (-> linked-list
        (assoc-in [first-node-id :previous-node-id] last-node-id)
        (assoc-in [last-node-id  :next-node-id] first-node-id))))

(defn shift-list
  [linked-list node-a-id node-b-id node-c-id node-d-id]
  (-> linked-list
      ;;a->c
      (assoc-in [node-a-id :next-node-id] node-c-id)
      ;;a<-c
      (assoc-in [node-c-id :previous-node-id] node-a-id)
      ;;c->b
      (assoc-in [node-c-id :next-node-id] node-b-id)
      ;;c<-b
      (assoc-in [node-b-id :previous-node-id] node-c-id)
      ;; b->d
      (assoc-in [node-b-id :next-node-id] node-d-id)
      ;; b<-d
      (assoc-in [node-d-id :previous-node-id] node-b-id)))

(defn move-left
  [node-c-id linked-list]
  ;; assuming original a->b->c->d
  ;; c and b swap - c is node to move, b is adjacent node
  ;; new order a->c->b->d
  ;; uuid is node c
  (let [{node-b-id :previous-node-id node-d-id :next-node-id} (get linked-list node-c-id)
        {node-a-id :previous-node-id} (get linked-list node-b-id)]
    (shift-list linked-list node-a-id node-b-id node-c-id node-d-id)))

(defn move-right
  [node-b-id linked-list]
  ;; assuming original a->b->c->d
  ;; b and c swap - b is node to move, c is adjacent node
  ;; new order a->c->b->d
  ;; uuid is node c
  (let [{node-a-id :previous-node-id node-c-id :next-node-id} (get linked-list node-b-id)
        {node-d-id :next-node-id} (get linked-list node-c-id)]
    (shift-list linked-list node-a-id node-b-id node-c-id node-d-id)))

(defn move
  [uuid linked-list modded-value]
  (cond (= 0 modded-value)
        linked-list
        (> 0 modded-value)
        (let [updated-list (move-left uuid linked-list)]
          (recur uuid updated-list (inc modded-value)))
        :else
        (let [updated-list (move-right uuid linked-list)]
          (recur uuid updated-list (dec modded-value)))))

(defn update-list
  [simple-mod? linked-list [uuid value]]
  (let [{:keys [next-node-id previous-node-id value]} (get linked-list uuid)
        modded-value                                  (mod-value value linked-list simple-mod?)]
    (move uuid linked-list modded-value)))


(defn decrypt
  [input simple-mod? & [decryption-key]]
  (let [processed-input (preprocess-numbers input decryption-key)
        linked-list (build-list processed-input)]
    (reduce (partial update-list simple-mod?) linked-list processed-input)))

(defn to-list
  [l]
  (let [[first-id v] (first l)]
    (loop [id (v :next-node-id) values [(v :value)]]
      (if (= id first-id)
        values
        (let [{:keys [value next-node-id]} (get l id)]
          (recur next-node-id (conj values value)))))))

(defn find-element-at-offset
  [linked-list starting-uuid offset]
  (let [node (get linked-list starting-uuid)]
    (if (= offset 0)
      node
      (recur linked-list (:next-node-id node) (dec offset)))))

(defn find-element-0
  [l]
  (first (filter (fn[[k {:keys [value]}]]
                   (= value 0))
                 l)))

(def decryption-key 811589153)

(defn find-values-offset-from-zero
  [l]
  (let [element-0 (first (find-element-0 l))
        v1 (find-element-at-offset l element-0 1000)
        v2 (find-element-at-offset l element-0 2000)
        v3 (find-element-at-offset l element-0 3000)]
    [[v1 v2 v3]
     (reduce (fn [agg {:keys [value]}] (+ agg value)) 0 [v1 v2 v3])]))

(defn part-1
  [i]
  (let [l (decrypt i)]
    (find-values-offset-from-zero l)))

(defn mix
  [order starting-list]
  (reduce (partial update-list false) starting-list order))

(defn decrypt-part-2
  [input mix-count & [decryption-key]]
  (let [processed-input (preprocess-numbers input decryption-key)
        linked-list (build-list processed-input)]
    (reduce (fn[agg _] (mix processed-input agg)) linked-list (range mix-count))))

(defn part-2
  [i]
  (let [l (decrypt-part-2 i 10 decryption-key)]
    (find-values-offset-from-zero l)))
