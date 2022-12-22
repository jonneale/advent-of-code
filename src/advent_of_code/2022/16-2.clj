(ns advent-of-code.2022.16-2
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :as set]))
(def input
  (->> (-> (io/resource "2022/16.txt")
           slurp
           (clojure.string/split  #"\n"))))


(def test-input
  ["Valve AA has flow rate=0; tunnels lead to valves DD, II, BB"
   "Valve BB has flow rate=13; tunnels lead to valves CC, AA"
   "Valve CC has flow rate=2; tunnels lead to valves DD, BB"
   "Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE"
   "Valve EE has flow rate=3; tunnels lead to valves FF, DD"
   "Valve FF has flow rate=0; tunnels lead to valves EE, GG"
   "Valve GG has flow rate=0; tunnels lead to valves FF, HH"
   "Valve HH has flow rate=22; tunnel leads to valve GG"
   "Valve II has flow rate=0; tunnels lead to valves AA, JJ"
   "Valve JJ has flow rate=21; tunnel leads to valve II"

   ])

(defn get-valve-key
  [row]
  (apply str (take 2 (drop 6 row))))

(defn get-flow-rate
  [row]
  (-> row
      (s/split #";")
      first
      (s/split #"=")
      last
      read-string))

(defn get-routes
  [row]
  (-> row
      (s/split #"lead(s)? to valve(s)? ")
      last
      (s/split #", ")))

(defn parse-input-row
  [row]
  (let [valve-key (get-valve-key row)
        flow-rate (get-flow-rate row)
        routes    (get-routes row)]
    {valve-key {:flow-rate flow-rate :routes routes}}))

(defn parse-input
  [input]
  (-> (->> input
           (map parse-input-row)
           (apply merge))
      (assoc :current-location "AA" :current-flow-rate 0 :time-left 30 :water-released 0 :open-valves #{})))

(defn state-as-graph
  [state]
  (apply merge
         (for [k (filter string? (keys state))]
           {k (get-in state [k :routes])})))

(defn get-flow-rates
  [state]
  (apply merge
         (for [k (filter string?(keys state))]
           {k (get-in state [k :flow-rate])})))

(defn i-to-g
  [input]
  (let [state (parse-input input)]
    {:routes (state-as-graph state)
     :flow-rates (get-flow-rates state)}))

(defn initial-unvisited
  [graph start]
  (assoc
   (apply merge
          (for [k (keys graph)]
            {k [Double/POSITIVE_INFINITY []]}))
   start
   [0 []]))

(defn get-cost
  [cost-map node]
  (first (get cost-map node)))

(defn get-unvisited
  [graph visited]
  (set/difference (set (keys graph))
                  (set visited)))

(defn cost-per-step
  [valve-values visited-nodes]
  (-
   (apply + (vals valve-values))
   (apply + (map #(get valve-values %) visited-nodes))))

(defn update-cost-entry
  [flow-rates visited cost-map new-node previous-node]
  (let [op-cost (cost-per-step flow-rates visited)
        step-cost (if (contains? (set visited) new-node) 1 2)
        [cost-to-this-point path-to-this-point] (cost-map previous-node)
        new-cost  (+ cost-to-this-point step-cost op-cost)]
    (if (> (first (cost-map new-node)) new-cost)
      (assoc cost-map new-node [new-cost (conj path-to-this-point previous-node)])
      cost-map)))

(defn di-step
  [graph flow-rates visited unvisited-nodes cost-map]
  (let [[next-node & _] (sort-by (partial get-cost cost-map) unvisited-nodes)
        nodes-from-here (get graph next-node)]
    [next-node (reduce (fn[cost-map new-node]
                         (update-cost-entry flow-rates visited cost-map new-node next-node))
                       cost-map
                       nodes-from-here)]))

(defn di-2
  [{:keys [routes flow-rates] :as graph} start-node]
  (loop [cost-map (initial-unvisited routes start-node)
         visited  []]
    (let [unvisited (get-unvisited routes visited)]
      (if (empty? unvisited)
        cost-map
        (let [[next-node updated-costs] (di-step routes flow-rates visited unvisited cost-map)]
          (recur updated-costs (conj visited next-node)))))))

(defn max-flow-rate?
  [[[_ flow-rate]]]
  (= 81 flow-rate))

(defn generate-new-states
  [graph [[current-node flow-rate] path cost]]
  (let [adjacent-nodes (graph current-node)]
    (for [node adjacent-nodes]
      [[node flow-rate] (conj path current-node) cost])))

(defn bfs
  [{:keys [routes flow-rates] :as graph} start-node]
  (loop [agenda [[start-node 0] [] 0]]
    (let [[first-element & remaining] (sort-by last agenda)]
      (if (max-flow-rate? first-element)
        first-element
        (generate-new-states graph first-element))
)))






(defn initial-facts
  [i]
  (let [parsed (parse-input i)
        str-keys (filter string?
                         (keys
                          parsed))]
    (select-keys parsed str-keys)))

(defn update-routes
  [all-routes [origin-route {:keys [flow-rate routes]}]]
  (reduce
   (fn [previous-routes [route cost]]
     (cond (= origin-route route)
           previous-routes
           (= 0 (get-in all-routes [route :flow-rate]))
           (concat previous-routes
                   (for [[new-route new-route-cost] (get-in all-routes [route :routes])]
                     [new-route (+ cost new-route-cost)]))
           :else
           (conj previous-routes [route cost])))
   []
   routes))

(defn initialise-route-costs
  [facts]
  (reduce (fn [agg [k {:keys [flow-rate routes]}]]
            (assoc-in agg [k :routes] (map #(vector % 1) routes)))
          facts facts))

(defn collapse-routes
  [k l]
  (remove (fn[[route-k route-cost]]
            (= route-k k))
          (map (comp first
                     (partial sort-by last))
               (vals (group-by first l)))))

(defn apply-route-costs-until-stable
  [updated-routes]
  (let [result (reduce (fn[agg [k v :as row]]
                         (assoc-in agg [k :routes] (collapse-routes k (update-routes agg [k v]))))
                       updated-routes
                       updated-routes)]
    (if true #_(= result updated-routes)
      result
      (recur result))))

(defn simplify-facts
  [facts]
  (initialise-route-costs facts)
  #_(let [initial-pass (initialise-route-costs facts)
        simple-costs (apply-route-costs-until-stable initial-pass)
        ]
    simple-costs
    ))



(defn get-valve-flow-rate
  [facts node]
  (:flow-rate (get facts node)))

(defn nodes-from-here
  [facts nodes]
  (:routes (get facts nodes)))




;; (def lowest-time (atom 31))

;; (defn pa
;;   [agenda {:keys [current-node current-flow-rate open-valves route time]}]
;;   (when (> @lowest-time time)
;;     (do (reset! lowest-time time)
;;         (println "current node " current-node)
;;         (println "current flow " current-flow-rate)
;;         (println "open-valves " open-valves)
;;         (println "route " route)
;;         (println "time " time)
;;         (println "-------------------------------")
;;         (println(count agenda))
;;         #_(println agenda)
;;         (println "-------------------------------")
;;         (println "")
;;         (println "")
;;         #_(Thread/sleep 1000)))
;;   )

;; (defn sort-fn
;;   [x]
;;   [(- 30 (:time x))
;;    (- 81 (:current-flow-rate x))])

;; (defn add-new-agenda-items
;;   [agenda new-items]
;;   #_(distinct (concat agenda new-items))
;;   (concat agenda
;;           (remove nil?
;;                   (let [previous-seen-routes (set (map #(conj (sort (:route %)) (:current-node %) (:open-valves %)) agenda))]
;;                     (for [new-agenda-item new-items]
;;                       (when-not (contains? previous-seen-routes (conj (sort (:route new-agenda-item))
;;                                                                       (:current-node new-agenda-item)
;;                                                                       (:open-valves new-agenda-item)))
;;                         new-agenda-item))))))

;; (defn prune-agenda
;;   [a]
;;   a
;;   #_(let [x (count a)]
;;     (if (> 200 x)
;;       a
;;       (take (int (* x 0.75)) a))))

;; (defn s
;;   [facts starting-node]
;;   (loop [agenda [{:current-node starting-node :current-flow-rate 0 :open-valves #{} :route [] :time 30}] results []]
;;     (let [[head-agenda-item & remaining-agenda] (prune-agenda (sort-by sort-fn agenda))]
;;       (do
;;           (pa agenda head-agenda-item)
;;           (cond
;;             (nil? head-agenda-item)
;;             results
;;             (or (= (:time head-agenda-item) 0) (= 81 (:current-flow-rate head-agenda-item)))
;;             (recur remaining-agenda (if (= 81 (:current-flow-rate  head-agenda-item))
;;                                       (conj results head-agenda-item)
;;                                       results))
;;             :else
;;             (let [new-agenda-items (generate-new-agenda-items facts head-agenda-item)]
;;               (recur (distinct (add-new-agenda-items remaining-agenda new-agenda-items))
;;                      results)))))))

(defn generate-new-agenda-items
  [facts {:keys [current-node current-flow-rate open-valves route time total-flowed]}]
  (concat
   (for [node (nodes-from-here facts current-node)]
     {:current-node node
      :current-flow-rate current-flow-rate
      :total-flowed (+ total-flowed current-flow-rate)
      :open-valves open-valves
      :route (conj route current-node)
      :time (dec time)})
   (if (or (contains? open-valves current-node) (= 0 (get-valve-flow-rate facts current-node)))
     []
     [{:current-node current-node
       :total-flowed (+ total-flowed current-flow-rate)
       :current-flow-rate (+ current-flow-rate (get-valve-flow-rate facts current-node))
       :open-valves (conj open-valves current-node)
       :route route
       :time (dec time)}])))

(defn expand-all-agenda-items
  [facts agenda]
  (apply concat
         (pmap
          (fn [head-agenda-item]
            (generate-new-agenda-items facts head-agenda-item))
          agenda)))

(defn complete?
  [{:keys [current-flow-rate]}]
  (= current-flow-rate 81))

(defn unique-key
  [{:keys [current-node route open-valves total-flowed]}]
  (conj (sort route)
        current-node
        total-flowed
        (sort open-valves)))

(defn prune
  [agenda]
  (pmap first (vals (group-by unique-key agenda)))
  #_(let [best (last (sort-by :total-flowed agenda))
        best-outcome (+ (:total-flowed best) (* time-left (:current-flow-rate best)))]
    (filter
     (fn[agenda-item]
       (< ()
        best-outcome))
     )))

(defn mc
  [facts starting-node]
  (loop [agenda [{:current-node starting-node :total-flowed 0 :current-flow-rate 0 :open-valves #{} :route [] :time 30}]
         time 0
         results []]
    (println "time " time)
    (println "size " (count agenda))
    (if (= time 26)
      results
      (let [next-steps (expand-all-agenda-items facts agenda)
            unique-next-steps (prune next-steps)
            results    (filter complete? unique-next-steps)
            remaining  (remove complete? unique-next-steps)]
        (recur remaining (inc time) results)))))


(defn expand-agenda
  [{:keys [current-node total-flowed current-flow-rate open-valves route time] :as this-agenda-item} facts]
  (let [valve-node (if (or (contains? (set open-valves) current-node) (= 0 (get-in facts [current-node :flow-rate])))
                     []
                     [(-> this-agenda-item
                          (update :open-valves (partial cons current-node))
                          (update :current-flow-rate (partial + (get-in facts [current-node :flow-rate])))
                          (update :time dec)
                          (update :total-flowed (partial + current-flow-rate)))])
        routes-from-here (get-in facts [current-node :routes])
        move-nodes  (for [[route-node cost] routes-from-here]
                      (-> this-agenda-item
                          (assoc :current-node route-node)
                          (update :time #(- % cost))
                          (update :route (partial cons current-node))
                          (update :total-flowed (partial + current-flow-rate))))
        all-nodes   (concat valve-node move-nodes)]
    (remove empty? all-nodes)))


(defn deduplicate
  [a]
  (distinct a))

(defn pret
  [a i]
  (when (= 0 (mod i 1000))
    (do
      (println (count a))
      (println "time of first " (:time (first a)))
      (println "flow rate first " (:current-flow-rate (first a)))
      (println "---------------------------------")
      (Thread/sleep 30))))

(defn bfs-bii
  [input starting-node]
  (let [simple-facts (simplify-facts (initial-facts input))]
    (loop [agenda [{:current-node starting-node :total-flowed 0 :current-flow-rate 0 :open-valves #{} :route [] :time 30}] results [] i 0]
      (pret agenda i)
      (let [[agenda-head & agenda-tail] (sort-by (juxt :current-flow-rate :time) agenda)]
        (if (or (not agenda-head) (<= (:time agenda-head) 0))
          agenda-head
          #_(concat results agenda)
          (let [next-steps (deduplicate (expand-agenda agenda-head simple-facts))
                results    (filter complete? next-steps)
                remaining  (remove complete? next-steps)]
            (recur (concat agenda-tail remaining) results (inc i)))
          )))))

(defn fw-direct
  [input]
  (let [all-keys (keys input)]
    (apply merge
           (for [from-key all-keys]
             (let [self-route {[from-key from-key] 0}
                   routes-from-here (get-in input [from-key :routes])]
               (merge self-route
                      (apply merge
                             (for [[route cost] routes-from-here]
                               {[from-key route] cost}))))))))

(defn floyd-warshall
  [input]
  (let [direct-routes (fw-direct input)]
    (reduce
     (fn [found-routes [i j k]]
       (let [i_to_j (get found-routes [i j] Double/POSITIVE_INFINITY)
             i_to_k (get found-routes [i k] Double/POSITIVE_INFINITY)
             k_to_j (get found-routes [k j] Double/POSITIVE_INFINITY)]
         (if (> i_to_j (+ i_to_k k_to_j))
           (assoc found-routes [i j] (+ i_to_k k_to_j))
           found-routes)))
     direct-routes
            (for [i (keys input) j (keys input) k (keys input)]
              [k j i]))))

(def test-data
  {"1" {:routes [["3" -2]]}
   "2" {:routes [["1" 4] ["3" 3]]}
   "3" {:routes [["4" 2]]}
   "4" {:routes [["2" -1]]}})

(defn unvisited-nodes
  [input starting-node]
  (let [non-zero-keys (map first (remove (fn[[k v]] (= 0 (:flow-rate v))) input))]
    (set/difference (set non-zero-keys) #{starting-node})))

(defn find-all-routes
  "search from node to all pairs. from all next nodes to all subs pairs etc. produce all solutions."
  [input starting-node]
  (let [simple-input (simplify-facts (initial-facts input))]
    (loop [agenda [{:current-node starting-node :unvisited-nodes (unvisited-nodes simple-input starting-node) :path [starting-node]}] result []]
      (let [[agenda-head & agenda-tail] (sort-by (comp count :unvisited-nodes) agenda)]
        (cond (nil? agenda-head)
              (map :path result)
              (empty? (:unvisited-nodes agenda-head))
              (recur agenda-tail (conj result agenda-head))
              :else
              (let [new-routes (for [new-node (:unvisited-nodes agenda-head)]
                                 {:current-node new-node
                                  :unvisited-nodes (set/difference (set (:unvisited-nodes agenda-head))
                                                                   #{new-node})
                                  :path (conj (:path agenda-head) new-node)})]
                (recur (concat agenda-tail new-routes) result)))))))

(defn calculate-route-cost
  [input route-costs route]
  (reduce (fn [[from-node flow-rate total-flowed time-left] to-node]
            (let [cost-step (inc (get route-costs [from-node to-node]))
                  flow-rate-change (get-in input [to-node :flow-rate])]
              [to-node (+ flow-rate flow-rate-change) (+ total-flowed (* flow-rate cost-step)) (- time-left cost-step)]))
          [(first route) 0 0 30]
          (rest route)))

(defn final-cost
  [[_ flow-rate current-total time-left]]
  (+ current-total (* flow-rate time-left)))

(defn cost-of-routes
  [input starting-node]
  (let [routes (find-all-routes input starting-node)
        simple-input (simplify-facts (initial-facts input))
        cost-map (floyd-warshall simple-input)
        costs-with-all-open-valves (map (partial calculate-route-cost simple-input cost-map) routes)]
    (sort (map final-cost costs-with-all-open-valves))))
