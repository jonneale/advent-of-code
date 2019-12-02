(ns advent-of-code.2019.1
  (:require [clojure.string :as s]
            [clojure.java.io :as io]))

(def data (s/split (slurp (io/resource "2019/1.txt")) #"\n"))

(defn to-ints
  [data]
  (map #(Integer/parseInt %) data))

(defn calculate-fuel-required
  [mass]
  (max 0 (- (Math/floor (/ mass 3)) 2)))

(defn calculate-fuel-including-mass-of-fuel
  ([mass]
   (calculate-fuel-including-mass-of-fuel mass 0))
  ([mass fuel-so-far]
   (if (>= 0 mass)
     fuel-so-far
     (let [additional-fuel (calculate-fuel-required mass)]
       (calculate-fuel-including-mass-of-fuel additional-fuel (+ fuel-so-far additional-fuel))))))

(defn calculate-total-fuel-required
  [d]
  (reduce +
          (map calculate-fuel-required (to-ints d))))

(defn calculate-total-fuel-required-including-mass-of-fuel
  [d]
  (reduce +
          (map calculate-fuel-including-mass-of-fuel (to-ints d))))
