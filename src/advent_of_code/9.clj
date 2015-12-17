(ns advent-of-code.9
  (:require [clojure.string :as s]))

(def test-input
  ["London to Dublin = 464"
   "London to Belfast = 518"
   "Dublin to Belfast = 141"])

(def input
  (s/split (slurp "resources/9.txt") #"\n"))

(defn- parse-line
  [l]
  (let [[from _ to _ distance] (s/split l #"\s")]
    [from to distance]))

(defn- build-network
  [input]
  (reduce (fn [network route]
            (let [[from to distance] (parse-line route)
                  int-distance (read-string distance)]

              (-> network
                  (assoc-in [from to] int-distance)
                  (assoc-in [to from] int-distance))))
          {} input))

(defn- routes-from-starting-city
  [network [city distance] [{:keys [route length]}] goal]
  (let [updated-route       (cons city route)
        route-with-distance [{:length (+ (or length 0) distance) :route updated-route}]]
    (if (= (sort updated-route) (sort goal))
      route-with-distance
      (remove nil?
              (mapcat (fn [new-city]
                        (when (not (contains? (set updated-route) (first new-city)))
                          (routes-from-starting-city network new-city route-with-distance goal)))
                      (network city))))))
(defn routes
  [network]
  (let [all-cities (keys network)]
    (reduce into
            (for [city all-cities]
              (routes-from-starting-city network [city 0] [] all-cities)))))

(defn shortest-route
  [input]
  (first (sort-by :length (routes (build-network input)))))

(defn longest-route
  [input]
  (last (sort-by :length (routes (build-network input)))))
