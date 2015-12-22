(ns advent-of-code.15)

(def ingredients
  {"sugar" {"capacity" 3, "durability" 0, "flavor" 0, "texture" -3, "calories" 2}
   "sprinkles" {"capacity" -3, "durability" 3, "flavor" 0, "texture" 0, "calories" 9}
   "candy" {"capacity" -1, "durability" 0, "flavor" 4, "texture" 0, "calories" 1}
   "chocolate" {"capacity" 0, "durability" 0, "flavor" -2, "texture" 2, "calories" 8}})

(defn scale-vals
  [m factor]
  (into {}
        (map (fn [[k v]] [k (* v factor)]) m)))

(defn min-zero-vals
  [m]
  (into {} (map (fn [[k v]] [k (max 0 v)]) m)))

(defn all-cookies
  []
  (for [tbs-sugar     (range 1 101)
        tbs-sprinkles (range 1 101)
        tbs-candy     (range 1 101)
        tbs-chocolate (range 1 101)
        :when (and (= 100 (+ tbs-sugar tbs-sprinkles tbs-candy tbs-chocolate)))]
    (min-zero-vals
     (merge-with + (scale-vals (ingredients "sugar") tbs-sugar)
                 (scale-vals (ingredients "sprinkles") tbs-sprinkles)
                 (scale-vals (ingredients "candy") tbs-candy)
                 (scale-vals (ingredients "chocolate") tbs-chocolate)))))

(defn dissoc-calories
  [m]
  (dissoc m "calories"))

(defn sorted-cookies
  []
  (reverse (sort-by (comp (partial reduce *) vals dissoc-calories) (all-cookies))))

(defn sorted-500-calorie-cookies
  []
  (filter #(= (% "calories") 500) (sorted-cookies)))
