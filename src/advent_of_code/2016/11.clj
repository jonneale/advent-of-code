(ns advent-of-code.2016.11
  (:require [clojure.string :as s]))

(def test-input
  "The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.
The second floor contains a hydrogen generator.
The third floor contains a lithium generator.
The fourth floor contains nothing relevant.")

(def input
  (slurp "resources/2016/11.txt"))

(defn floor-state
  [state]
  (dissoc state :elevator))

(defn to-s [state]
  (let [floors (reverse (sort (keys (floor-state state))))
        all-elements (reduce concat (vals (floor-state state)))]
    (doseq [current-floor floors]
      (println 
       (apply str 
              (concat [(str "F" current-floor " " (if (= (:elevator state) current-floor) " E " " . "))]
                      (for [{:keys [type floor fuel]} all-elements]
                        (if (= current-floor floor) (str (first fuel) (first type) " ")
                            " . ")
                        )))))
    (println "")))

                        

(defn build-microchip [floor-number microchip-details]
  {:type "microchip"
   :fuel (first (s/split microchip-details #"-compatible"))
   :floor floor-number})

(defn build-generator [floor-number microchip-details]
  {:type "generator"
   :fuel (first (s/split microchip-details #" "))
   :floor floor-number})

(defn parse-floor-contents [floor-contents floor-number]
  (concat (map (comp (partial build-microchip floor-number) first) (re-seq #"([^\s]*-compatible [a-z]*)" floor-contents))
          (map (comp (partial build-generator floor-number) first) (re-seq #"([^\s]* generator)" floor-contents))))

(defn parse-input [input]
  (assoc 
   (->> (s/split input #"\n")
        (map-indexed (fn [ix floor-contents]
                       {ix (parse-floor-contents floor-contents ix)}))
        (apply merge))
   :elevator 0))

(defn winning-state? [state]
  (let [floors (sort (keys (floor-state state)))]
    (reduce (fn [empty-so-far? current-floor] (and empty-so-far? (empty? (state current-floor)))) 
            true
            (butlast floors))))


(defn comb [s]
   (loop [[f & r] (seq s) p '(())]
      (if f (recur r (concat p (map (partial cons f) p)))
            p)))

(defn is-element? [elem-1 elem-2]
  (let [elem-1-type (:type elem-1)
        elem-1-fuel (:fuel elem-1)
        elem-2-type (:type elem-2)
        elem-2-fuel (:fuel elem-2)]
    (and (= elem-1-type elem-2-type)
         (= elem-1-fuel elem-2-fuel))))

(defn update-state [state direction-to-move elements-to-move]
  (let [elevator-moved (update state :elevator #(+ direction-to-move %))]
    (reduce (fn [current-state element-to-move]
              (let [floor-element-started-on (:floor element-to-move)
                    floor-element-ends-on    (+ floor-element-started-on direction-to-move)]
                (-> current-state
                    (update floor-element-started-on #(remove (partial is-element? element-to-move) %))
                    (update floor-element-ends-on    #(conj % (assoc element-to-move :floor floor-element-ends-on)))))) 
            elevator-moved 
            elements-to-move)))

(defn all-possible-moves [state]
  (let [elevator-floor             (state :elevator)
        floor-elements             (state elevator-floor)]
    (for [direction-to-move [-1 1]
          elements-to-move  (remove #(or (empty? %) (> (count %) 2)) (comb floor-elements))]
      (update-state state direction-to-move elements-to-move))))

(defn no-generators-present? [elements]
   (empty? (filter #(= (:type %) "generator") elements)))

(defn every-microchip-present-has-a-generator? [elements]
  (let [microchips (filter #(= "microchip" (:type %)) elements)
        generators (filter #(= "generator" (:type %)) elements)]
    (every? true? (map (fn [microchip]
                         (let [chip-fuel (:fuel microchip)]
                           (not (empty? (filter #(= chip-fuel (:fuel %)) generators))))) microchips))))

(defn no-microchips-destroyed? [elements]
  (or (no-generators-present? elements)
      (every-microchip-present-has-a-generator? elements)))

(defn valid-move? [current-state new-state]
  (and (= (set (keys current-state))
          (set (keys new-state)))
       (every? true? (map no-microchips-destroyed? (vals (floor-state new-state))))))

(defn add-history
  [history possible-moves]
  (map (fn [state] {:state state :history (inc history)}) possible-moves))

(defn previously-seen? [previous-states current-state]
  (not (empty? (filter #(= % current-state) previous-states))))

(defn search-for-path-to-victory
  [initial-state]
  (loop [possible-next-moves [{:state initial-state :history 0}] all-previously-seen-states [] ]
    (let [[current-state-and-history & other-moves] possible-next-moves
          {current-state :state history :history} current-state-and-history]
      (if (winning-state? current-state)
        current-state-and-history
        (let [future-states (->> current-state
                                 all-possible-moves
                                 (filter (partial valid-move? current-state))
                                 (remove (partial previously-seen? all-previously-seen-states))
                                 (add-history history)
                                 (concat other-moves))]

          (recur future-states (conj all-previously-seen-states )))))))
