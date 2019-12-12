(ns advent-of-code.2019.6
  (:require [clojure.string :as string]
            [clojure.java.io :as io]))

(def test-input
  "B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)G
E)J
J)K
K)L
COM)B")

(def t2
  "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
K)YOU
I)SAN")

(def input
  (slurp (io/resource "2019/6.txt")))

(defn parse-input
  [s]
  (map #(string/split % #"\)") (string/split s #"\n")))

(defn orbits
  ([x all-orbits]
   (orbits x all-orbits []))
  ([x all-orbits indirect-orbits]
   (if-let [direct-parent (:direct (all-orbits x))]
     (orbits direct-parent all-orbits (conj indirect-orbits direct-parent))
     indirect-orbits)))

(defn generate-orbits
  [all-orbits [x y]]
  (assoc all-orbits y {:direct x
                       :indirect (orbits x all-orbits)}))

(defn input->orbits
  [i]
  (let [interim (reduce generate-orbits {} (parse-input i))]
    (reduce generate-orbits interim (parse-input i))))

(defn add-connection
  [map from to]
  (if (map from)
    (update-in map [from] (partial cons to))
    (assoc map from [to])))

(defn generate-map
  [agg [from to]]
  (-> agg
      (add-connection from to)
      (add-connection to from)))

(defn input->map
  [i]
  (reduce generate-map {} (parse-input i)))

(defn search-for-route
  ([map from to]
   (search-for-route map from to #{}))
  ([map from to so-far]
   ;; (Thread/sleep 500)
   ;; (println "From " from)
   ;; (println "To " to)
   (println "So Far " so-far)
   (if (= from to)
     so-far
     (let [potential-new-points (clojure.set/difference (set (map from)) (set so-far))]
       (remove empty?
               (flatten
                (for [new-starting-point potential-new-points]
                  (search-for-route map new-starting-point to (conj so-far new-starting-point)))))))))

(defn route-has-cycle?
  [route]
  (false? (every? #(= % 1) (vals (frequencies route)))))

(defn search-for-route
  ([m from to]
   (search-for-route m from to  #{[from]}))
  ([m from to agenda]
   (if (empty? agenda)
     "no route found"
     (let [[current-route & remaining-agenda] (sort-by count agenda)
           [next-point & _] current-route]
       (if (= next-point to)
         current-route
         (let [possible-next-points-from-this-point   (m next-point)
               possible-new-routes                    (reduce (fn [agg point]
                                                                (let [new-route (cons point current-route)]
                                                                  (if (route-has-cycle? new-route)
                                                                    agg
                                                                    (conj agg new-route))))
                                                              (or agenda #{})
                                                              possible-next-points-from-this-point)]
           (search-for-route m from to (clojure.set/difference possible-new-routes #{current-route}))))))))


;; (defn search-for-path
;;   [input from to]
;;   (-
;;    (count
;;     (first
;;      (sort-by count
;;               (-> input
;;                   input->map
;;                   (search-for-route from to)))))
;;    2 ;;subtract 2 as the start and end node are included
;;    ))

(defn search-for-path
  [input from to]
  (-> input
      input->map
      (search-for-route from to)
      (count)
      (- 3)))

(defn count-orbits
  [i]
  (let [orbits (input->orbits i)
        direct-count (count (map :direct orbits))
        indirect-count (reduce + (map #(-> % :indirect count) (vals orbits)))]
    (+ direct-count indirect-count)))
