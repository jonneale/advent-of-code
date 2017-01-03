(ns advent-of-code.2016.10
  (:require [clojure.string :as s]))

(defn assign-value
  [state target v]
  (if-let [existing-low-value (get-in state [target "low"])]
    (if (> existing-low-value v)
      (assoc state target {"low" v "high" existing-low-value})
      (assoc state target {"low" existing-low-value "high" v}))
    (assoc-in state [target "low"] v)))

(defn setup
  [commands]
  (reduce 
   (fn [state command]
     (let [[c v _ _ target-part-1 target-part-2] (s/split command #" ")]
       (if (= c "value")
         (let [int-v (Integer/parseInt v)]
           (assign-value state (str target-part-1 target-part-2) int-v))
         state)))
   {}
   commands))

(def input
  (slurp "resources/2016/10.txt"))

(defn value-from-bot
  [state [bot bot-bucket]]
  (and (get-in state [bot "low"]) 
       (get-in state [bot "high"]) 
       (get-in state [bot bot-bucket])))

(defn process-commands
  [initial-state all-commands]
  (loop [state initial-state commands all-commands]
    (if (empty? commands)
      state
      (let [[command-to-process & remaining-commands] commands
            {:keys [from to]}                         command-to-process]
        (if-let [microchip-to-move (value-from-bot state from)]
          (recur (assign-value state to microchip-to-move) remaining-commands)
          (recur state (concat remaining-commands [command-to-process])))))))

(def test-input
  "value 5 goes to bot 2
bot 2 gives low to bot 1 and high to bot 0
value 3 goes to bot 1
bot 1 gives low to output 1 and high to bot 0
bot 0 gives low to output 2 and high to output 0
value 2 goes to bot 2")

(defn build-command
  [from-1 from-2 element to-1 to-2]
  {:from [(str from-1 from-2) element]
   :to   (str to-1 to-2)})

(defn transform-command
  [command]
  (let [[command initiator _ element _ target-1-part-1 target-1-part-2 & [more? element-2 _ target-2-part-1 target-2-part-2]] (s/split command #" ")]
    (when (not= "value" command)
      (let [command-1 (build-command command initiator element target-1-part-1 target-1-part-2)]
        (if more?
          [(build-command command initiator element-2 target-2-part-1 target-2-part-2)
           command-1]
          [command-1])))))

(defn transform-commands
  [commands]
  (mapcat transform-command commands))

(defn run
  [input]
  (let [commands (s/split input #"\n")
        transformed-commands (transform-commands commands)]
    (-> commands
        setup
        (process-commands transformed-commands))))

(defn find-bot-which-compares
  [input value-a value-b]
  (filter (fn [x] (= (sort [value-a value-b]) 
                     (sort (vals (last x))))) 
          (run input)))

;; part 1
;; (find-bot-which-compares input 61 17)

;; part 2
;; (let [output (run input)]
;;   (reduce * (mapcat (fn [k] (vals (output k))) ["output0" "output1" "output2"])))
