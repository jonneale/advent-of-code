(ns advent-of-code.2022.19
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))
(def input
    (->> (-> (io/resource "2022/19.txt")
              slurp
             (clojure.string/split  #"\n"))))

(def test-input
  ["Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian."
   "Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian"])

(defn parse-costs
  [c]
  (reduce (fn[agg [amount type]]
            (conj agg [type (read-string amount)])) [] (partition 2 (remove #(= "and" %) c))))

(defn robot-spec
  [row]
  (let [[_ _ robot-type _ _ & raw-cost] (s/split row #" ")]
    {robot-type (parse-costs raw-cost)}))

(defn costs-in-ore
  [blueprints element]
  (reduce +
          (for [[cost-element cost] (get blueprints element)]
            (if (= "ore" cost-element)
              cost
              (* cost (costs-in-ore blueprints cost-element))))))

(defn calc-costs
  [blueprints]
  (apply merge
         (for [k (keys blueprints)]
           {k (costs-in-ore blueprints k)})))

(defn parse-blueprints
  [rows]
  (for [row rows]
    (let [[id spec] (s/split row #":")
          blueprints (apply merge (map robot-spec (s/split spec #"\.")))]
      {:id (last (s/split id #" "))
       :blueprints blueprints
       :costs (calc-costs blueprints)})))

(def initial-state
  {"time" 0
   "clay" 0
   "obsidian" 0
   "geode" 0
   "ore" 0
   :score 0
   "geode-bots" 0
   "robots" ["ore"]})

(defn can-build?
  [state cost]
  (last
   (reduce (fn[[state buildable?] [type cost]]
                 (if (and buildable?
                          (>= (get state type) cost))
                   [(update state type #(- % cost)) true]
                   [state false]))
               [state true] cost)))

(defn buildables
  [state blueprints]
  (remove nil?
          (for [[type cost] blueprints]
            (if (can-build? state cost)
              type
              nil))))

(defn score-state
  [costs state]
  (let [robots (get state "robots")
        geode-bots (get state "geode-bots")
        robot-cost (* geode-bots geode-bots (reduce + (map #(get costs %) robots)))]
    robot-cost
    (+ robot-cost

       (* (get costs "ore")
          (get state "ore"))
       (* (get costs "clay")
          (get state "clay"))
       (* (get costs "obsidian")
          (get state "obsidian"))
       (* (get costs "geode")
          (get state "geode")
          100000000))))

(defn apply-robots
  [costs state]
  (reduce (fn[new-state robot-type]
            (-> new-state
                (update robot-type inc)
                (update :score #(let [scale-factor (if (= robot-type "geode") 10 1)]
                                  (+ % (* scale-factor (get costs robot-type)))))))
          state
          (get state "robots")))

(defn reduce-stock-by-cost
  [state cost]
  (reduce (fn[new-state [type item-cost]]
            (update new-state type #(- % item-cost))) state cost))

(defn build-robot
  [state robot blueprints]
  (let [robot-cost (get blueprints robot)]
    (-> state
        (reduce-stock-by-cost robot-cost)
        (update "robots" #(sort (conj % robot)))
        (update "geode-bots" #(if (= robot "geode") (inc %) %)))))

(defn prioritise-geode
  [things-we-can-build blueprints costs]
  things-we-can-build
  (cond (contains? (set things-we-can-build) "geode")
        ["geode"]
        (contains? (set things-we-can-build) "obsidian")
        ["obsidian"]
        :else
        things-we-can-build))

(defn proceed-one-tick
  [state blueprints costs]
  (let [all-things-we-can-build (buildables state blueprints)
        things-we-can-build     (prioritise-geode all-things-we-can-build blueprints costs)
        state-after-drilling    (apply-robots costs state)
        new-state (update state-after-drilling "time" inc)]
    (into [new-state]
          (for [robot things-we-can-build]
            (build-robot new-state robot blueprints)))))

(defn prune-states
  [t costs states]
  (take 5000 (reverse (sort-by :score states))))

(defn run-n-ticks
  [initial-state blueprints n]
  (reduce (fn[agg t]
            (let [prune-fn (if (> t 19) prune-states identity)]
              (prune-fn (distinct (apply concat (pmap #(proceed-one-tick % blueprints) agg)))))) [initial-state] (range n)))

(defn process-blueprint
  [i blueprint-index & [time-limit]]
  (let [blueprints (parse-blueprints i)
        blueprint (nth blueprints blueprint-index)]
    (doall
     (loop [states [initial-state] current-time 1]
       (println (str current-time " - " (count states)))
       (if (= current-time (or time-limit 25))
         states
         (let [new-states (->> states
                               (pmap (fn[state] (proceed-one-tick state (:blueprints blueprint) (:costs blueprint))))
                               (apply concat)
                               distinct
                               (prune-states current-time (:costs blueprint)))]
           (recur new-states
                  (inc current-time))))))))

(defn pb
  [i index]
  (time (do (process-blueprint i index) nil)))

(defn part-1
  [i]
  (let [blueprints (parse-blueprints i)]
    (doall (for [index (range (count blueprints))]
             [(inc index)              (reduce max (map #(get % "geode")
                                                        (process-blueprint i index)))]))))

(defn part-2
  [i]
  (let [blueprints (take 3 (parse-blueprints i))]
    (doall (for [index (range (count blueprints))]
             [(inc index)    (reduce max (map #(get % "geode")
                                              (process-blueprint i index 33)))]))))

(defn sum-part-1
  [i]
  (reduce + (map (partial apply *) (part-1 i))))
