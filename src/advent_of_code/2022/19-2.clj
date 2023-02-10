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
