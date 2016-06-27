(ns advent-of-code.22.2)

(def initial-game-state
  {:player {:mana       500
            :hit-points 50
            :defense    0}
   :boss   {:damage     10
            :hit-points 71}
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

(declare poison)
(declare recharge)
(declare shield)

(def shield-effect-fn
  {:turns 6
   :name "shield       "
   :effect identity
   :parent shield
   :initial-event (fn [state]
                    (update-in state [:player :defense] #(+ % 7)))
   :final-event (fn [state]
                  (update-in state [:player :defense] #(- % 7)))})

(def poison-fn
  {:turns 6
   :name "poison      -"
   :parent poison
   :effect (damage-boss-fn 3)
   :final-event identity
   :initial-event identity})

(def recharge-fn
  {:turns 5
   :name "recharge    -"
   :parent recharge
   :effect (fn [state]
             (update-in state [:player :mana] #(+ % 101)))
   :final-event identity
   :initial-event identity})

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
        initial-effects    (filter (comp nil? :effect-applied) effects)
        expiring-effects   (filter #(zero? (:turns %)) effects)
        continuing-effects (remove #(zero? (:turns %)) effects)
        state-with-initial-effects-applied (reduce (fn [agg-state effect] ((:initial-event effect) agg-state)) state initial-effects)
        state-with-expired-events-applied (reduce (fn [agg-state effect] ((:final-event effect) agg-state)) state-with-initial-effects-applied expiring-effects)]
    (update-in 
     (reduce apply-effect state-with-expired-events-applied continuing-effects)
     [:effects]
     #(remove nil? (map (fn [effect]
                          (when-not (zero? (:turns effect))
                            (-> effect
                                (assoc :effect-applied true)
                                (update-in [:turns] dec)))) %)))))

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
  (let [remaining-mana (-> state :player :mana)
        current-effects (set (map (comp :name :parent) (remove #(< (:turns %) 2) (-> state :effects))))]
    (filter #(and (not (current-effects (:name %)))
                  (< (:cost %) remaining-mana)) actions)))

(defn calculate-boss-damage
  [state]
  (max 1
       (- (-> state :boss :damage)
          (-> state :player :defense))))

(defn boss-attack
  [state]
  (update-in state [:player :hit-points] #(- % (calculate-boss-damage state))))

(def max-turns 40)

(defn check-end-state
  [state]
  (cond (:result state)
        state
        (<= (-> state :boss :hit-points) 0)
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

(defn decrease-player-hp
  [state hard-mode?]
  (if hard-mode?
    (update-in state [:player :hit-points] dec)
    state))

(defn player-turn
  [state action hard-mode?]
  (-> state
      (decrease-player-hp hard-mode?)
      check-end-state
      apply-delayed-effects
      (apply-action action)
      (update-in [:turns] inc)
      check-end-state))

(defn boss-turn
  [state]
  (-> state
      apply-delayed-effects
      (boss-attack)
      (update-in [:turns] inc)
      check-end-state))

(defn advance-one-tick
  [state action hard-mode?]
  (-> state
      (player-turn action hard-mode?)
      boss-turn))

(defn run
  [state hard-mode?]
  (flatten
   (for [action (filter-possible-actions state)]
     (let [new-state (advance-one-tick state action hard-mode?)]
       (if-let [result (:result new-state)]
         (if (= :player-wins result)
           new-state
           nil)
         (run new-state hard-mode?))))))

(defn find-quick-wins
  [initial-state hard-mode?]
  (first
   (sort-by (fn [final-state]
              (reduce + (map :cost (:history final-state))))
            (filter #(= :player-wins (:result %)) (run initial-state hard-mode?)))))

(defn print-player-state
  [state action]
  (let [player (:player state)]
    (println "-- Player Turn --")
    (println "- Player has " (player :hit-points) " hit points, " (player :defense) " armor, " (player :mana) " mana")
    (println "- Boss has " (-> state :boss :hit-points) " hit points")
    (doseq [effect (:effects state)]
      (println (:name effect) "; its timer is now " (:turns effect)))
    (println "Player casts " (:name action))
    (println "")
    (println "")))

(defn print-boss-state
  [state]
  (let [player (:player state)]
    (println "-- Boss Turn --")
    (println "- Player has " (player :hit-points) " hit points, " (player :defense) " armor, " (player :mana) " mana")
    (println "- Boss has " (-> state :boss :hit-points) " hit points")
    (doseq [effect (:effects state)]
      (println (:name effect) "; its timer is now " (:turns effect)))
    (println "Boss attacks for  " (calculate-boss-damage state) " damage!")
    (println "")
    (println "")))

        
(defn run-sequence
  [initial-state actions hard-mode?]
  (reduce (fn [agg-state action]
            
            (let [post-player-turn-state (player-turn agg-state action hard-mode?)
                  new-state              (boss-turn post-player-turn-state)]
              (Thread/sleep 1000)
              (print-player-state post-player-turn-state action)
              (print-boss-state new-state)
              new-state))
          initial-state
          actions))




