(ns advent-of-code.2022.7
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def test-input
  (s/split "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k" #"\n"))

(def input
    (->> (-> (io/resource "2022/7.txt")
             slurp
             (clojure.string/split  #"\n"))))

(defn process-change-dir
  [state instruction]
  (cond (= instruction "$ cd ..")
        (update-in state [:current-dir] #(butlast %))
        (= instruction "$ cd /")
        (assoc state :current-dir ["/"])
        :else (let [dir-name (last (s/split instruction #"\$ cd "))
                    new-state (update-in state [:current-dir] #(conj (vec %) dir-name))]
                (if (get-in new-state (new-state :current-dir))
                  new-state
                  (assoc-in new-state (new-state :current-dir) {dir-name {:size 0}})))))

(defn process-instruction
  [state instruction]
  (if (.startsWith instruction "$ cd")
    (process-change-dir state instruction)
    state))

(defn add-directory
  [state instruction]
  (let [dir-name (last (s/split instruction #"dir "))
        current-dir (:current-dir state)]
    (assoc-in state (conj current-dir dir-name) {:size 0})))

(defn add-file-with-size
  [state instruction]
  (update-in state (conj (state :current-dir) :size)
             #(let [size (read-string (first (s/split instruction #" ")))] (+ size %))))

(defn update-state
  [state instruction]
  (cond (.startsWith instruction "$")   (process-instruction state instruction)
        (.startsWith instruction "dir") (add-directory state instruction)
        :else                           (add-file-with-size state instruction)))

(defn process
  [state instructions]
  (let [[instruction & remaining] instructions]
    (if (nil? instruction)
      state
      (recur (update-state state instruction) remaining))))

(def initial-state
  {:current-dir ["/"] :size 0 "/" {:size 0}})

(defn parse
  [input]
  (reduce update-state initial-state input))


(defn calculate-all-subdir-sizes
  [dir-tree]
  (let [subfolders (filter string? (keys dir-tree))
        this-dir-size (:size dir-tree)]
    (if (empty? subfolders)
      this-dir-size
      (+ this-dir-size (reduce + (map #(calculate-dir-size (dir-tree %)) subfolders))))))


(defn update-tree
  [dir-tree]
  (let [subfolders (filter string? (keys dir-tree))
        updated-tree (assoc dir-tree :size (calculate-all-subdir-sizes dir-tree))]
    (if (empty? subfolders)
      updated-tree
      (reduce #(assoc %1 %2 (update-tree (dir-tree %2))) updated-tree subfolders))))

(defn get-all-sizes-from-tree
  [dir-tree]
  (let [this-level-size (:size dir-tree)
        subfolders (filter string? (keys dir-tree))]
    (into [this-level-size] (mapcat #(get-all-sizes-from-tree (dir-tree %)) subfolders))))

(defn solve-part-1
  [i]
  (->> i
       (parse)
       update-tree
       get-all-sizes-from-tree
       (filter #(> 100000 %))
       (reduce +)))


(defn solve-part-2
  [i]
  (let [all-sizes         (->> i
                               (parse)
                               update-tree
                               get-all-sizes-from-tree
                               sort
                               butlast)
        total-used-space  (last all-sizes)
        necessary-free-space (- 30000000 (- 70000000 total-used-space))]
    (first (drop-while #(> necessary-free-space %) all-sizes))))
