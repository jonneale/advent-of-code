(ns advent-of-code.1)

(def input (clojure.string/trim-newline (slurp "resources/1.txt")))

(def mask
  {\( 1
   \) -1})

(defn- all-floors
  [input]
  (reductions + (map mask input)))

(defn which-floor
  [input]
  (last (all-floors input)))

(defn first-basement
  [input]
  (inc (count (take-while (comp not neg?) (all-floors input)))))
