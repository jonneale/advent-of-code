(ns advent-of-code.2022.16
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

(defn advance-time-one-min
  [{:keys [current-flow-rate water-released] :as state}]
  (-> state
      (update :time-left dec)
      (update :water-released #(+ % current-flow-rate))))

(defn open-this-valve-state
  [{:keys [open-valves current-flow-rate current-location] :as state}]
  (-> state
      (advance-time-one-min)
      (update :open-valves #(conj % current-location))
      (update :current-flow-rate #(+ % (-> (get state current-location) :flow-rate)))))

(defn move-to
  [state route]
  (-> state
      (advance-time-one-min)
      (assoc :current-location route)))

(defn generate-new-states
  [{:keys [current-location open-valves] :as state}]
  (let [open-this-valve (when (not (contains? (set open-valves) current-location))
                          (open-this-valve-state state))
        possible-locations (map (partial move-to state) ((get state current-location) :routes))]
    (remove nil? (conj possible-locations open-this-valve))))

(defn print-state
  [{:keys [current-location current-flow-rate time-left open-valves]} rest]
  (println "*******************************")
  (println "current location: " current-location)
  (println "flow-rate: " current-flow-rate)
  (println "open-valves: " open-valves)
  (println "time-left: " time-left)
  (println "agenda-size " (count rest))
  (Thread/sleep 3000)
  )

(defn dissoc-keys
  [new-state]
  (-> new-state
      (dissoc :time-left)
      (dissoc :water-released)))

(defn find-new-states
  [new-states previous-states]
  (remove #(not (empty? (set/intersection previous-states #{(dissoc-keys %)}))) new-states))

(defn update-previous-states
  [previous-states new-state]
  (set (conj previous-states (dissoc-keys new-state))))



(defn available-flow-rate-here
  [{:keys [current-location open-valves] :as state}]
  (if (contains? (set open-valves) current-location)
    0
    ((state current-location) :flow-rate)))

(defn sort-agenda
  [agenda]
  (reverse
   (sort-by
    (juxt :current-flow-rate
          available-flow-rate-here
          #(count (:open-valves %)))
    agenda)))

(defn grid->costs
  [grid start]
  (assoc
   (apply merge
          (map (comp (partial apply hash-map) #(vector % Double/POSITIVE_INFINITY)) (keys grid)))
   start
   0))

(defn get-legal-neighbours
  [node graph]
  (get graph node))

(defn to-map
  [x]
  (apply merge (map (partial apply hash-map) x)))

(defn update-unvisited
  [visited unvisited cost-to-this-point neighbour-costs]
  (reduce
   (fn[cost-map [neighbour cost]]
     (let [new-cost (+ cost cost-to-this-point)]
       (cond (contains? (set (map first visited)) neighbour)
             cost-map
             (> new-cost (cost-map neighbour))
             cost-map
             :else
             (assoc cost-map neighbour new-cost))))
   (to-map unvisited)
   neighbour-costs))

(defn d-step
  [unvisited visited graph]
  (let [[[first-node cost] & others] (reverse (sort-by last unvisited))
        updated-visited (vec (conj visited [first-node cost]))
        neighbour-costs (get-legal-neighbours first-node graph)
        neighbours (keys neighbour-costs)
        new-neighbours             (clojure.set/difference (set neighbours) (set (map first updated-visited)))]
    [(update-unvisited updated-visited others cost neighbour-costs) updated-visited]))

(defn initial-unvisited
  [graph start]
  (assoc
   (apply merge
          (for [k (keys graph)]
            {k Double/POSITIVE_INFINITY}))
   start
   0))

(defn dijkstras-algo
  [graph start]
  (last
   (let [unvisited (reverse (sort-by last (initial-unvisited graph start)))]
     (reduce (fn[[unvisited visited] _]
               (d-step unvisited visited graph))
             [unvisited []] unvisited))))

(defn graph-for-single-node
  [node state]
  (apply merge
         (let [routes-from-here ((get state node) :routes)]

           (for [route routes-from-here]
             {route ((get state route) :flow-rate)}))))

(defn state-as-graph
  [state]
  (apply merge
         (for [k (filter string? (keys state))]
           {k
            (graph-for-single-node k state)})))
;; (defn d-step
;;   [unvisited visited grid & [reverse]]
;;   (let [[[first-node cost] & rest] (sort-by last unvisited)
;;         updated-visited (vec (conj visited [first-node cost]))
;;         neighbours (get-legal-neighbours first-node grid reverse)
;;         new-neighbours (clojure.set/difference (set neighbours) (set (map first updated-visited)))]
;;     [(update-unvisited rest cost new-neighbours) updated-visited]))


;; (defn dijkstras-algo
;;   [grid start & [reverse]]
;;   (last
;;    (let [initial-unvisited (grid->costs grid start)]
;;      (reduce (fn[[unvisited visited] _]
;;                (d-step unvisited visited grid reverse))
;;              [initial-unvisited []] initial-unvisited))))


(defn initial-visited-list
  [graph start-node]
  (assoc
   (apply merge  (map #(hash-map % false) (keys graph)))
   start-node
   true))

(def test-graph
  {"0" {"1" 1 "2" 1}
   "1" {"0" 1}
   "2" {"0" 1}})

(def test-2
  {"0" {"1" 1 "2" 1 "3" 1}
   "1" {"0" 1}
   "2" {"0" 1}
   "3" {"0" 1}})

(defn update-visited
  [visited-list k]
  (assoc visited-list k true))

(defn some-unvisited?
  [visited]
  (not (every? true? (vals visited))))

(defn cost
  [graph path]
  (second
   (or
    (reduce (fn[[current-node current-cost] next-node]
[next-node (+ current-cost (get (get graph (str current-node)) (str next-node)))])
            [(first path) 0] (rest path))
    0)))

(defn path-cycles?
  [path]
  (and (> (count path) 3)
       (let [[a b c d] (take 4 (reverse path))]
         (and (= a c)
              (= b d)))))
(defn only-uniques
  [graph agenda]
  (map (fn [[k v]] (conj k v))
       (reduce (fn[agg [start visited path]]
                 (if (path-cycles? path)
                   agg
                   (if-let [visited-path (agg [start visited])]
                     (if (> (cost graph visited-path) (cost graph path))
                       (assoc agg [start visited] path)
                       (assoc agg [start visited] visited-path))
                     (assoc agg [start visited] path))))
               {}
               agenda)))
(defn p[agenda]
  (clojure.pprint/pprint agenda)
  (println "")
  (println "")
  (println "")
  (Thread/sleep 500))

(defn bfs
  [graph start-node]
  (loop [agenda #{[start-node (initial-visited-list graph start-node) []]}]
    (p agenda)

    (let [[[current-node visited path] & remaining] (sort-by (comp (partial cost graph) last) agenda)]
      (if (and current-node (some-unvisited? visited))
        (let [options (keys (get graph current-node))
              new-agenda (concat remaining
                                 (for [k options]
                                   [k (update-visited visited k) (conj path current-node)]))]
          (recur (only-uniques graph new-agenda)))
        [current-node visited (conj path current-node)]))))

(def example
  {"S" {"E" 8 "A" 10}
   "A" {"C" 2}
   "B" {"A" 1}
   "C" {"B" -2}
   "D" {"C" -1 "A" -4}
   "E" {"D" 1}})

(defn update-cost
  [graph cost-map k]
  (let [cost-here (cost-map k)
        routes-from-k (graph k)]
    (if (= cost-here Double/POSITIVE_INFINITY)
      cost-map
      (reduce (fn[cost-map [destination cost]]
                (let [new-cost (+ cost-here cost)]
                  (if (> (cost-map destination) new-cost)
                    (assoc cost-map destination (+ cost-here cost))
                    cost-map)))
              cost-map routes-from-k))))

(defn bellman
  [graph start-node]
  (loop [costs (initial-unvisited graph start-node) iteration 0]
    (let [new-costs (reduce (partial update-cost graph) costs (keys costs))]
      (if (or (= new-costs costs) (= iteration (dec (count(keys costs)))))
        costs
        (recur new-costs (inc iteration))))))

(defn initialise-unvisited
  [graph start]
  (assoc
   (apply merge
          (for [k (keys graph)]
            {k [Double/POSITIVE_INFINITY []]}))
   start
   [0 []]))

(defn update-cost-2
  [graph cost-map k]
  (let [[cost-here route-here] (cost-map k)
        routes-from-k (graph k)]
    (if (= cost-here Double/POSITIVE_INFINITY)
      cost-map
      (reduce (fn[cost-map [destination cost]]
                (let [new-cost (+ cost-here cost)]
                  (if (> (first (cost-map destination)) new-cost)
                    (assoc cost-map destination [(+ cost-here cost) (conj route-here k)])
                    cost-map)))
              cost-map
              routes-from-k))))

(defn bellman-2
  [graph start-node]
  (loop [costs (initialise-unvisited graph start-node) iteration 0]
    (let [new-costs (reduce (partial update-cost-2 graph) costs (keys costs))]
      (if (or (= new-costs costs) (= iteration (dec (count(keys costs)))))
        costs
        (recur new-costs (inc iteration))))))

(def di-example
  {"A" {"B" 4 "C" 2}
   "B" {"D" 2 "C" 3 "E" 3}
   "C" {"B" 1 "E" 5 "D" 4}
   "D" {}
   "E" {"D" 1} })

(defn get-cost
  [cost-map node]
  (get cost-map node))

(defn get-unvisited
  [graph visited]
  (set/difference (set (keys graph))
                  (set visited)))

(defn di-step
  [graph unvisited-nodes cost-map]
  (let [[next-node & _] (sort-by (partial get-cost cost-map) unvisited-nodes)
        nodes-from-here (get graph next-node)]
    [next-node (reduce (fn[cost-map [new-node cost]]
                         (assoc cost-map new-node cost))
                       cost-map
                       nodes-from-here)]))

(defn di-2
  [graph start-node]
  (loop [cost-map (initial-unvisited graph start-node)
         visited  []]
    (let [unvisited (get-unvisited graph visited)]
      (if (empty? unvisited)
        cost-map
        (let [[next-node updated-costs] (di-step graph unvisited cost-map)]
          (recur updated-costs (conj visited next-node)))))))
