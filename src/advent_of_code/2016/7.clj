(ns advent-of-code.2016.7)

(def input
  (clojure.string/split (slurp "resources/2016/7.txt") #"\n"))

(defn split-line
  [input]
  (partition-by #(nil? (re-find #"[a-z]" (str %))) input))

(defn parse-input
  [input]
  (map split-line input))

(defn contains-abba?
  [characters]
  (some (fn [s]
              (and (= s (reverse s))
                   (not= (first s) (second s))))
            (partition 4 1 characters)))

(defn may-contain-abba
  [characters]
  (contains-abba? characters))

(defn must-not-contain-abba
  [characters]
  (when (contains-abba? characters)
    false))

(def tls-check-fns
  ;; we have a string which is [non-hypernet-seq delimiter hypernet-seq delimiter] repeated
  [may-contain-abba (constantly nil) must-not-contain-abba (constantly nil)])

(defn check-line
  [line]
  (remove nil? (map (fn [c f] (f c)) line (cycle tls-check-fns))))

(defn ip-supports-tls?
  [line]
  (let [line-results (check-line line)]
    (and (not (empty? line-results))
         (every? true? line-results))))

(defn aba-seqs
  [characters]
  (filter (fn [s]
            (and (= s (reverse s))
                 (not= (first s) (second s))))
          (partition 3 1 characters)))

(defn any-blocks-equivalent?
  [aba-blocks bab-blocks]
  (clojure.set/intersection (set aba-blocks) (set (map (fn [[b a _]] [a b a]) bab-blocks))))

(defn ip-supports-ssl?
  [line]
  (let [supernet-sequences (take-nth 4 line)
        hypernet-sequences (take-nth 4 (drop 2 line))
        aba-blocks         (mapcat aba-seqs supernet-sequences)
        bab-blocks         (mapcat aba-seqs hypernet-sequences)]
    (not (empty? (any-blocks-equivalent? aba-blocks bab-blocks)))))

(defn count-ssl-ips
  [input]
  (count (filter ip-supports-ssl? (parse-input input))))

(defn count-supported-ips
  [input]
  (count (filter ip-supports-tls? (parse-input input))))
