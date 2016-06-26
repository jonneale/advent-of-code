(ns advent-of-code.22.2)

(def initial-game-state
  {:player {:mana       250
            :hit-points 10
            :defense    0}
   :boss   {:damage     8
            :hit-points 14}
   :effects []
   :history []
   :turns   0})

(defn damage-boss-fn
  [damage]
  (fn [state]
    (update-in state [:boss :hit-points]
               #(- % damage))))

(defn heal-player-fn
  [health]
  (fn [state]
    (update-in state [:player :hit-points]
               #(+ % health))))

(def shield-effect-fn
  {:turns 6
   :name "shield       "
   :effect identity
   :final-event (fn [state]
                  (update-in state [:player :defense] #(- % 7)))})

(def poison-fn
  {:turns 6
   :name "poison      -"
   :effect (damage-boss-fn 3)
   :final-event identity})

(def recharge-fn
  {:turns 5
   :name "recharge    -"
   :effect (fn [state]
              (update-in state [:player :mana] #(+ % 101)))
   :final-event identity})

(def magic-missile
  {:cost 53
   :duration 1
   :name "magic missile   -"
   :effect (damage-boss-fn 4)})

(def drain
  {:cost 73
   :duration 1
   :name "drain           -"
   :effect (comp (damage-boss-fn 2) (heal-player-fn 2))})

(def shield
  {:cost 113
   :name "shield          -"
   :effect (fn [state]
             (-> state
                 (update-in [:player :defense] #(+ % 7))
                 (update-in [:effects] #(conj % shield-effect-fn))))})

(def poison
  {:cost 173
   :name "poison          -"
   :effect (fn [state]
             (update-in state [:effects] #(conj % poison-fn)))})

(def recharge
  {:cost 229
   :name "recharge        -"
   :effect (fn [state]
             (update-in state [:effects] #(conj % recharge-fn)))})

(defn boss-hp
  [state]
  (-> state :boss :hit-points))

(defn apply-effect
  [state action]
  ((:effect action) state))

(defn apply-delayed-effects
  [state]
  (let [effects (:effects state)
        expiring-effects   (filter #(zero? (:turns %)) effects)
        continuing-effects (remove #(zero? (:turns %)) effects)
        state-with-expired-events-applied (reduce (fn [agg-state effect] ((:final-event effect) agg-state)) state expiring-effects)]
    (update-in 
     (reduce apply-effect state-with-expired-events-applied continuing-effects)
     [:effects]
     #(remove nil? (map (fn [effect] (when-not (zero? (:turns effect)) (update-in effect [:turns] dec))) %)))))

(defn apply-action
  [original-game-state action-this-turn]
  (-> original-game-state
      (update-in [:player :mana] #(- % (:cost action-this-turn)))
      (update-in [:history] #(conj % action-this-turn))
      (apply-effect action-this-turn)))

(def actions
  [shield magic-missile drain recharge poison])

(defn filter-possible-actions
  [state]
  (let [remaining-mana (-> state :player :mana)]
    (filter #(< (:cost %) remaining-mana) actions)))

(defn boss-attack
  [state]
  (update-in state [:player :hit-points] #(- % (max 1
                                                    (- (-> state :boss :damage)
                                                       (-> state :player :defense))))))

(def max-turns 8)

(defn check-end-state
  [state]
  (cond (<= (-> state :boss :hit-points) 0)
        (assoc state :result :player-wins)
        (<= (-> state :player :hit-points) 0)
        (assoc state :result :boss-wins)
        (empty? (filter-possible-actions state))
        (assoc state :result :player-out-of-mana)
        (> (:turns state) max-turns)
        (assoc state :result :time-out)
        :else
        state))

(defn print-state
  [state]
  (println "---------------------------------")
  (println "- player      hp " (-> state :player :hit-points) "           -")
  (println "-           mana " (-> state :player :mana) "          -")
  (println "-        defense " (-> state :player :defense)   "            -")
  (println "-                               -")
  (println "- boss hp   " (-> state :boss :hit-points) "                -")
  (println "-                               -")
  (println "- Effects:                      -")
  (doseq [effect (:effects state)]
    (println "- " (:name effect) "     turns: " (:turns effect)  "-"))
  (println "- History:                      -")
  (doseq [action (:history state)]
    (println "-              " (:name action)))
  (println "---------------------------------"))

(defn advance-one-tick
  [state action]
  (-> state
      apply-delayed-effects
      (apply-action action)
      apply-delayed-effects
      (boss-attack)
      (update-in [:turns] inc)
      check-end-state))

(def cheapest-winner
  (atom nil))

(defn cost
  [state]
  (reduce + (map :cost (:history state))))

(defn run
  [state]
  (flatten
   (for [action (filter-possible-actions state)]
     (let [new-state (advance-one-tick state action)]
       (if (and @cheapest-winner (> (cost new-state)
                                    (cost @cheapest-winner)))
         nil
         (if-let [result (:result new-state)]
           (if (= :player-wins result)
             (reset! cheapest-winner new-state)
             nil)
           (run new-state)))))))

(defn find-quick-wins
  [initial-state]
  (run initial-state))


(defn run-sequence
  [initial-state actions]
  (do
    (print-state initial-state)
    (reduce (fn [agg-state action]
              (let [new-state (advance-one-tick agg-state action)]
                (Thread/sleep 1000)                                  
                (print-state new-state)
                new-state))
            initial-state
            actions)
    nil))
