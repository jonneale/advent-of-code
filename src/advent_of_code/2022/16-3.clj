(ns advent-of-code.2022.16-3
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

(def test-case-1
  ["Valve AA has flow rate=0; tunnels lead to valves BA"
   "Valve BA has flow rate=2; tunnels lead to valves AA, CA"
   "Valve CA has flow rate=4; tunnels lead to valves BA, DA"
   "Valve DA has flow rate=6; tunnels lead to valves CA, EA"
   "Valve EA has flow rate=8; tunnels lead to valves DA, FA"
   "Valve FA has flow rate=10; tunnels lead to valves EA, GA"
   "Valve GA has flow rate=12; tunnels lead to valves FA, HA"
   "Valve HA has flow rate=14; tunnels lead to valves GA, IA"
   "Valve IA has flow rate=16; tunnels lead to valves HA, JA"
   "Valve JA has flow rate=18; tunnels lead to valves IA, KA"
   "Valve KA has flow rate=20; tunnels lead to valves JA, LA"
   "Valve LA has flow rate=22; tunnels lead to valves KA, MA"
   "Valve MA has flow rate=24; tunnels lead to valves LA, NA"
   "Valve NA has flow rate=26; tunnels lead to valves MA, OA"
   "Valve OA has flow rate=28; tunnels lead to valves NA, PA"
   "Valve PA has flow rate=30; tunnels lead to valves OA"])

(def test-case-2
  ["Valve AA has flow rate=0; tunnels lead to valves AB, BB, CB"
"Valve AB has flow rate=0; tunnels lead to valves AA, AC"
"Valve AC has flow rate=0; tunnels lead to valves AB, AD"
"Valve AD has flow rate=0; tunnels lead to valves AC, AE"
"Valve AE has flow rate=0; tunnels lead to valves AD, AF"
"Valve AF has flow rate=0; tunnels lead to valves AE, AG"
"Valve AG has flow rate=0; tunnels lead to valves AF, AH"
"Valve AH has flow rate=0; tunnels lead to valves AG, AI"
"Valve AI has flow rate=0; tunnels lead to valves AH, AJ"
"Valve AJ has flow rate=0; tunnels lead to valves AI, AK"
"Valve AK has flow rate=100; tunnels lead to valves AJ, AW, AX, AY, AZ"
"Valve AW has flow rate=10; tunnels lead to valves AK"
"Valve AX has flow rate=10; tunnels lead to valves AK"
"Valve AY has flow rate=10; tunnels lead to valves AK"
"Valve AZ has flow rate=10; tunnels lead to valves AK"
"Valve BB has flow rate=0; tunnels lead to valves AA, BC"
"Valve BC has flow rate=0; tunnels lead to valves BB, BD"
"Valve BD has flow rate=0; tunnels lead to valves BC, BE"
"Valve BE has flow rate=0; tunnels lead to valves BD, BF"
"Valve BF has flow rate=0; tunnels lead to valves BE, BG"
"Valve BG has flow rate=0; tunnels lead to valves BF, BH"
"Valve BH has flow rate=0; tunnels lead to valves BG, BI"
"Valve BI has flow rate=0; tunnels lead to valves BH, BJ"
"Valve BJ has flow rate=0; tunnels lead to valves BI, BK"
"Valve BK has flow rate=100; tunnels lead to valves BJ, BW, BX, BY, BZ"
"Valve BW has flow rate=10; tunnels lead to valves BK"
"Valve BX has flow rate=10; tunnels lead to valves BK"
"Valve BY has flow rate=10; tunnels lead to valves BK"
"Valve BZ has flow rate=10; tunnels lead to valves BK"
"Valve CB has flow rate=0; tunnels lead to valves AA, CC"
"Valve CC has flow rate=0; tunnels lead to valves CB, CD"
"Valve CD has flow rate=0; tunnels lead to valves CC, CE"
"Valve CE has flow rate=0; tunnels lead to valves CD, CF"
"Valve CF has flow rate=0; tunnels lead to valves CE, CG"
"Valve CG has flow rate=0; tunnels lead to valves CF, CH"
"Valve CH has flow rate=0; tunnels lead to valves CG, CI"
"Valve CI has flow rate=0; tunnels lead to valves CH, CJ"
"Valve CJ has flow rate=0; tunnels lead to valves CI, CK"
"Valve CK has flow rate=100; tunnels lead to valves CJ, CW, CX, CY, CZ"
"Valve CW has flow rate=10; tunnels lead to valves CK"
"Valve CX has flow rate=10; tunnels lead to valves CK"
"Valve CY has flow rate=10; tunnels lead to valves CK"
"Valve CZ has flow rate=10; tunnels lead to valves CK"])


(def test-case-3
  ["Valve BA has flow rate=2; tunnels lead to valves AA, CA"
   "Valve CA has flow rate=10; tunnels lead to valves BA, DA"
   "Valve DA has flow rate=2; tunnels lead to valves CA, EA"
   "Valve EA has flow rate=10; tunnels lead to valves DA, FA"
   "Valve FA has flow rate=2; tunnels lead to valves EA, GA"
   "Valve GA has flow rate=10; tunnels lead to valves FA, HA"
   "Valve HA has flow rate=2; tunnels lead to valves GA, IA"
   "Valve IA has flow rate=10; tunnels lead to valves HA, JA"
   "Valve JA has flow rate=2; tunnels lead to valves IA, KA"
   "Valve KA has flow rate=10; tunnels lead to valves JA, LA"
   "Valve LA has flow rate=2; tunnels lead to valves KA, MA"
   "Valve MA has flow rate=10; tunnels lead to valves LA, NA"
   "Valve NA has flow rate=2; tunnels lead to valves MA, OA"
   "Valve OA has flow rate=10; tunnels lead to valves NA, PA"
   "Valve PA has flow rate=2; tunnels lead to valves OA, AA"
   "Valve AA has flow rate=0; tunnels lead to valves BA, PA"])

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

(defn get-flow-rates
  [state]
  (apply merge
         (for [k (filter string?(keys state))]
           {k (get-in state [k :flow-rate])})))

(defn initial-unvisited
  [graph start]
  (assoc
   (apply merge
          (for [k (keys graph)]
            {k [Double/POSITIVE_INFINITY []]}))
   start
   [0 []]))

(defn initial-facts
  [i]
  (let [parsed (parse-input i)
        str-keys (filter string?
                         (keys
                          parsed))]
    (select-keys parsed str-keys)))

(defn initialise-route-costs
  [facts]
  (reduce (fn [agg [k {:keys [flow-rate routes]}]]
            (assoc-in agg [k :routes] (map #(vector % 1) routes)))
          facts facts))

(defn simplify-facts
  [facts]
  (initialise-route-costs facts))

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

(defn pp
  [a i]
  (when (= 0 (mod i 100000))
    (println (first a))
    (println "most expensive " (reduce max (map :cost a)))
    (println "least expensive " (reduce min (map :cost a)))))


(defn find-all-routes
  "search from node to all pairs. from all next nodes to all subs pairs etc. produce all solutions."
  [simple-input cost-map starting-node ]
  (loop [agenda [{:current-node starting-node :unvisited-nodes (unvisited-nodes simple-input starting-node) :path [starting-node] :cost 0}] result [] i 0]
    (let [[agenda-head & agenda-tail] (sort-by (comp count :unvisited-nodes) agenda)]
      (pp agenda i)
      (cond (nil? agenda-head)
            (do (println "all done")
                result)
            (empty? (:unvisited-nodes agenda-head))
            (recur agenda-tail (conj result agenda-head) (inc i))
            :else
            (let [new-routes (for [new-node (:unvisited-nodes agenda-head)]
                               {:current-node new-node
                                :unvisited-nodes (set/difference (set (:unvisited-nodes agenda-head))
                                                                 #{new-node})
                                :cost (+ (:cost agenda-head) (inc (get cost-map [(:current-node agenda-head) new-node])))
                                :path (conj (:path agenda-head) new-node)})
                  too-expensive-nodes (filter (comp #(> % 30) :cost) new-routes)
                  only-legal-nodes    (remove (comp #(> % 30) :cost) new-routes)
                  updated-result      (if (not (empty? too-expensive-nodes)) (conj result agenda-head) result)]
              (recur (concat agenda-tail only-legal-nodes) updated-result (inc i)))))))

(defn calculate-route-cost
  [input route-costs route & [minutes]]
  (reduce (fn [[from-node flow-rate total-flowed time-left] to-node]
            (let [cost-step (inc (get route-costs [from-node to-node]))
                  flow-rate-change (get-in input [to-node :flow-rate])]
              [to-node (+ flow-rate flow-rate-change) (+ total-flowed (* flow-rate cost-step)) (- time-left cost-step)]))
          [(first route) 0 0 (or minutes 30)]
          (rest route)))

(defn final-cost
  [[_ flow-rate current-total time-left]]
  (+ current-total (* flow-rate time-left)))

(defn add-total-costs
  [input route-costs routes]
  (pmap
   (fn [{:keys [path cost] :as route}]
     (let [[_ flow-rate current-total time-left] (calculate-route-cost input route-costs route)]
       (assoc route :total-flow (final-cost [nil flow-rate current-total time-left]))))
   routes))

(defn cost-of-routes
  [input starting-node]
  (let [simple-input (simplify-facts (initial-facts input))
        cost-map (floyd-warshall simple-input)
        resulting-routes (find-all-routes simple-input cost-map starting-node)
        routes           (pmap :path resulting-routes)
        costs-with-all-open-valves (pmap (partial calculate-route-cost simple-input cost-map) routes)]
    (sort (pmap final-cost costs-with-all-open-valves))))

(defn p2
  [agenda i]
  (when (zero? (mod i 5000))
    (println i)
    (println (str (new java.util.Date)))
    (println (count agenda))
    (println (reduce max (map :elephant-time-left agenda)))
    (println (reduce min (map :elephant-time-left agenda)))
    (println (reduce max (map :my-time-left agenda)))
    (println (reduce min (map :my-time-left agenda)))
    (clojure.pprint/pprint (take 5 agenda))
    (println "----------------------------------------------------"))

  )

(defn find-all-routes-with-elephant
  [simple-input cost-map starting-node]
  (loop [agenda [{:my-current-node starting-node
                  :elephant-current-node starting-node
                  :unvisited-nodes (unvisited-nodes simple-input starting-node)
                  :path [[starting-node] [starting-node]]
                  :my-time-left 26
                  :elephant-time-left 26}]
         result [] i 0]
    (let [[{:keys [path my-time-left my-current-node elephant-time-left elephant-current-node unvisited-nodes] :as agenda-head} & agenda-tail] agenda]
      (p2 agenda i)
      (cond (nil? agenda-head)
            result
            (empty? unvisited-nodes)
            (recur agenda-tail (conj result agenda-head) (inc i))
            :else
            (let [new-routes (for [my-new-node       unvisited-nodes
                                   elephant-new-node unvisited-nodes
                                   :when             (and (not= my-new-node elephant-new-node)
                                                          (> my-time-left (cost-map [my-current-node my-new-node]))
                                                          (> elephant-time-left (cost-map [elephant-current-node elephant-new-node])))]
                               {:my-current-node my-new-node
                                :elephant-current-node elephant-new-node
                                :unvisited-nodes (set/difference (set unvisited-nodes)
                                                                 #{my-new-node elephant-new-node})
                                :my-time-left (- my-time-left (inc (get cost-map [my-current-node my-new-node])))
                                :elephant-time-left (- elephant-time-left (inc (get cost-map [elephant-current-node elephant-new-node])))
                                :path [(-> path first (conj my-new-node))
                                       (-> path last (conj elephant-new-node))]})
                  updated-result     (conj result agenda-head)]
              (recur (doall (concat agenda-tail new-routes)) updated-result (inc i)))))))


(defn update-results
  [results new-results]
  (reduce
   (fn[agg {:keys [visited-nodes total-flow]}]
     (if-let [old-v (get agg visited-nodes)]
       (if (> total-flow old-v)
         (assoc agg visited-nodes total-flow)
         agg)
       (assoc agg visited-nodes total-flow)))
   results
   new-results))

(defn bfs-2
  [simple-input cost-map starting-node & [starting-time]]
  (println starting-time)
  (let [all-nodes (unvisited-nodes simple-input starting-node)]
    (loop [agenda [{:current-node "AA" :time-left (or starting-time 30) :total-flow 0 :visited-nodes #{}}] results {}]
      (let [[{:keys [current-node visited-nodes total-flow time-left] :as agenda-head} & agenda-tail] agenda]
        (if (nil? agenda-head)
          results
          (let [new-nodes (for [node all-nodes
                                :when (and (not= node current-node)
                                           (not (contains? visited-nodes node))
                                           (> time-left (inc (cost-map [current-node node]))))]
                            (let [new-time-left (dec (- time-left (cost-map [current-node node])))]
                              {:current-node node
                               :time-left    new-time-left
                               :total-flow (+ total-flow (* new-time-left (get-in simple-input [node :flow-rate])))
                               :visited-nodes (set (conj visited-nodes node))}))
                updated-results (update-results results new-nodes)]
            (recur (doall (concat agenda-tail new-nodes)) updated-results)))))))

(defn part-1
  [input]
  (let [simple-input (simplify-facts (initial-facts input))
        cost-map (floyd-warshall simple-input)]
    (apply max (vals (bfs-2 simple-input cost-map "AA")))))

(defn total-disjoint-sets-for-this-route
  [[route this-total] all-routes]
  (remove zero?
          (pmap
           (fn [[other-route other-total]]
             (if (empty? (set/intersection route other-route))
               (+ this-total other-total)
               0))
           all-routes)))

(defn find-disjoint-sets
  [routes]
  (apply max
         (pmap (fn [route-and-total]
                 (apply max (total-disjoint-sets-for-this-route route-and-total routes)))
               routes)))

(defn part-2
  [input]
  (let [simple-input (simplify-facts (initial-facts input))
        cost-map (floyd-warshall simple-input)]
    (find-disjoint-sets (bfs-2 simple-input cost-map "AA" 26))))

(defn final-cost-with-elephant
  [simple-input cost-map [elephant-path my-path]]
  (let [elephant-path-cost (final-cost (calculate-route-cost simple-input cost-map elephant-path 26))
        my-path-cost (final-cost (calculate-route-cost simple-input cost-map my-path 26))]
    (+ elephant-path-cost my-path-cost)))

(defn cost-of-routes-with-elephant
  [input starting-node]
  (let [simple-input (simplify-facts (initial-facts input))
        cost-map (floyd-warshall simple-input)
        resulting-routes (find-all-routes-with-elephant simple-input cost-map starting-node)
        routes           (pmap :path resulting-routes)]
    (sort (pmap (partial final-cost-with-elephant simple-input cost-map) routes))))


(defn solve-part-1
  []
  (last (cost-of-routes test-input "AA")))
