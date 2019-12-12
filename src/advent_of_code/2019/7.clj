(ns advent-of-code.2019.7
  (:require [clojure.string :as s]
            [clojure.java.io :as io]))

(def i "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0")
(def i1 "3,23,3,24,1002,24,10,24,1002,23,-1,23,
101,5,23,23,1,24,23,23,4,23,99,0,0")

(def i2 "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5")

(def i2-2 "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10")

(def input (slurp (io/resource "2019/7.txt")))

(def position-mode \0)
(def immediate-mode \1)
(def halt "halt")

(defn waiting?
  [amp]
  (true? (:waiting amp)))

(defn halt?
  [amp]
  (true? (:halt amp)))

(defn tokenize-input
  [i]
  (apply merge
         (map-indexed (fn [i x] {i (Integer/parseInt x)})
                      (s/split (s/replace i #"\n" "") #","))))

(defn get-params
  [i position param-modes]
  (map-indexed (fn [ix mode]
                 (if (= mode immediate-mode)
                   (i (+ position (inc ix)))
                   (i (i (+ position (inc ix))))))
               param-modes))

(defn quick-maffs
  [operator opcode command i position]
  (let [param-modes (drop (count opcode) (reverse (format "%04d" command)))
        params      (get-params i position param-modes)
        value       (apply operator params)
        output-position (i (+ position 3))
        next-position   (+ position 4)]
    (-> i
        (assoc output-position value)
        (assoc :position next-position))))

(defn lt
  [x y]
  (if (< x y) 1 0))

(defn eq
  [x y]
  (if (= x y) 1 0))

(defmulti process-opcode
  (fn [opcode & _]
    opcode))

(defmethod process-opcode "01"
  [opcode command i position]
  (quick-maffs + opcode command i position))

(defmethod process-opcode "02"
  [opcode command i position]
  (quick-maffs * opcode command i position))


(defmethod process-opcode "03"
  [opcode command i position]
  (let [output-location (i (inc position))
        [input & remaining] (:input i)
        new-state (assoc i :input remaining)]
    (if (nil? input)
      (assoc i :waiting true)
      (-> new-state (assoc output-location input) (update-in [:position] (partial + 2))))))

(defmethod process-opcode "04"
  [opcode command i position]
  (let [param-modes                   (drop (count opcode)  (reverse (format "%03d" command)))
        [output-value]                (get-params i position param-modes)]
    (-> i (assoc :output output-value) (update-in [:position] (partial + 2)))))

(defn jump-if
  [predicate opcode command i position]
  (let [param-modes                   (drop (count opcode)  (reverse (format "%04d" command)))
        [value-to-test jump-location] (get-params i position param-modes)
        next-position                 (if (predicate value-to-test) jump-location (+ position 3))]
    (assoc i :position next-position)))

(defmethod process-opcode "05"
  [opcode command i position]
  (jump-if (comp not zero?) opcode command i position))

(defmethod process-opcode "06"
  [opcode command i position]
  (jump-if zero? opcode command i position))

(defmethod process-opcode "07"
  [opcode command i position]
  (quick-maffs lt opcode command i position))

(defmethod process-opcode "08"
  [opcode command i position]
  (quick-maffs eq opcode command i position))

(defmethod process-opcode :default
  [_ _ i & _]
  (assoc i :halt true))

(defn process-command
  [i]
  (let [position    (i :position)
        command     (i position)
        str-command (format "%02d" command)
        opcode      (subs str-command (- (count str-command) 2) (count str-command))]
    (process-opcode opcode command i position)))

(defn process-input
  []
;; ([i]
;;    (process-input i))
;;   ([i position]
;;    (println "position " position)
;;    (println  (sort-by first (dissoc (dissoc i :input) :output)))
;;    (Thread/sleep 300)
;;    (let [output  (process-command i position)]
;;      (if (= new-position halt)
;;        (:output i)
;;        (recur output new-position))))

  )

(defn add-input-value
  [amp value]
  (if (seq (:input amp))
    (update-in amp [:input] #(conj % value))
    (assoc amp :input [value])))

(defn build-feedback-amplifier
  [phase-input input i]
  (assoc i :input [phase-input input]))

(defn process-until-halt-or-waiting
  [i]
  (let [output (process-command i)]
    (if (or (waiting? output) (halt? output))
       output
       (recur output))))

(def debug false)
(defn print-state
  [i o other]
  (when debug
    (do
      (println "INPUT WAS: " i)
      (println "AMP WITH ID: " (:id o))
      #_(doseq [x (range (- (count (vals o)) 2))]
        (println x ":"
                 (o x)))
      (println "********************************")
      (println "Remainging amps " (map (juxt :id :waiting :halt) other))
      (Thread/sleep 100))))

(defn run-amplifiers
  [phase-input i]
  (let [instruction-set (tokenize-input i)
        amplifiers      (map-indexed (fn [i p] (assoc (assoc (add-input-value instruction-set p)
                                                            :position 0)
                                                     :id i)) phase-input)]
    (loop [input 0 next-amp (first amplifiers) other-amps (rest amplifiers)]
      (let [current-amp-with-input (add-input-value next-amp input)
            current-amp            (assoc current-amp-with-input :waiting false)]
        (let [output (process-until-halt-or-waiting current-amp)]
          (print-state input output other-amps)
          (cond (and (halt? output) (empty? other-amps))
                (:output output)
                (halt? output)
                (recur (:output output) (first other-amps) (rest other-amps))
                (waiting? output)
                (recur (:output output) (first other-amps) (conj (into [] (rest other-amps)) output))
                :else
                "SOMETHING WENT WRONG"))))))

(defn permutations [colls]
  (if (= 1 (count colls))
    (list colls)
    (for [head colls
          tail (permutations (disj (set colls) head))]
      (cons head tail))))

(defn find-max-output
  [phase-input i]
  (reduce max
          (for [phase-input-perm (permutations phase-input)]
            (run-amplifiers phase-input-perm i))))

(defn answer-part-1
  []
  (find-max-output [0 1 2 3 4] input))
