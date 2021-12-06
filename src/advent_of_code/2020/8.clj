(ns advent-of-code-2020.8
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def test-data "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6")

(def input (slurp (io/resource "8.txt")))

(defn add-initial-values
  [i]
  (-> i
      (assoc :accumulator 0)
      (assoc :current-position 0)))

(defn parse-input [i]
  (add-initial-values
   (apply merge (map-indexed (fn [i v]
                               (let [[cmd offset] (s/split v #" ")]
                                 {i [cmd (read-string offset)]}))
                             (s/split i #"\n")))))

(defn already-run?
  [command]
  (= "ALREADY RUN" command))

(defn mark-as-run
  [{:keys [current-position] :as state}]
  (assoc state current-position "ALREADY RUN"))

(defn run-command
  [state [command offset]]
  (let [new-state (mark-as-run state)]
    (case command
      "nop" (update-in new-state [:current-position] inc)
      "acc" (-> new-state
                (update-in [:accumulator] (partial + offset))
                (update-in [:current-position] inc))
      "jmp" (update-in new-state [:current-position] (partial + offset)))))



(defn run-program
  [state]
  (let [current-position (:current-position state)
        command-to-run   (state current-position)]
    (cond
      (nil? command-to-run)
      (:accumulator state)
      (already-run? command-to-run)
      false
      :else
      (recur (run-command state command-to-run)))))


(defn fix-program
  [input]
  (let [max-instruction (reduce max (filter number? (keys input)))]
    (loop [i 0]
      (if (> i max-instruction)
        "No way to fix"
        (let [[command offset] (input i)
              new-state (case command
                          "acc" input
                          "jmp" (assoc input i ["nop" offset])
                          "nop" (assoc input i ["jmp" offset]))]
          (if-let [output (run-program new-state)]
            output
            (recur (inc i))))))))



(fix-program (parse-input input))




