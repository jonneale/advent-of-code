(ns advent-of-code.2)


(def input (slurp "resources/2.txt"))


;; l w h
;; 2*l*w 2*w*h 2*l*h

(defn- transform-input
  [parcel-dimensions]
  (for [single-parcel-dimensions (clojure.string/split parcel-dimensions #"\n")]
    (map #(Integer/parseInt %) (clojure.string/split single-parcel-dimensions #"x"))))

(defn- square-feet-for-parcel
  [[l w h]]
  (let [sides [(* 2 l w) (* 2 w h) (* 2 l h)]]
    (+ (reduce + sides) (/ (reduce min sides) 2.0))))

(defn- calculate-square-footage-for-all-parcels
  [parcel-sizes]
  (map square-feet-for-parcel (transform-input parcel-sizes)))

(defn calculate-total-square-feet
  [parcel-sizes]
  (reduce + (calculate-square-footage-for-all-parcels parcel-sizes)))

(defn- ribbon-length-for-parcel
  [dimensions]
  (let [[x y z] (sort dimensions)]
    (+ x x y y (* x y z))))

(defn- calculate-ribbon-length-for-all-parcels
  [parcel-sizes]
  (map ribbon-length-for-parcel (transform-input parcel-sizes)))

(defn calculate-total-ribbon-length
  [parcel-sizes]
  (reduce + (calculate-ribbon-length-for-all-parcels parcel-sizes)))
