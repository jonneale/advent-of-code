(ns advent-of-code.2020.22
  (:require [clojure.string :as s]
            [clojure.java.io :as io]))

(def input (slurp (io/resource "2020/22.txt")))

(def test-input
  "Player 1:
9
2
6
3
1

Player 2:
5
8
4
7
10")

(defn score-deck
  [deck]
  (reduce + (map-indexed (fn [ix i] (* (inc ix) i)) (reverse deck))))

(defn parse-deck
  [deck]
  (map read-string (rest (s/split deck #"\n"))))

(defn parse-input
  [i]
  (let [decks (s/split i #"\n\n")
        [player-1-deck player-2-deck] (map parse-deck decks)]
    [player-1-deck player-2-deck]))


(defn play-game
  [player-1-deck player-2-deck]
  (cond (empty? player-1-deck)
        (score-deck player-2-deck)
        (empty? player-2-deck)
        (score-deck player-1-deck)
        :else
        (let [[player-1-card & player-1-remaining-deck] player-1-deck
              [player-2-card & player-2-remaining-deck] player-2-deck]
          (if (> player-1-card player-2-card)
            (recur (concat player-1-remaining-deck [player-1-card player-2-card]) player-2-remaining-deck)
            (recur player-1-remaining-deck (concat player-2-remaining-deck [player-2-card player-1-card]))))))


(defn run-full-game
  [input]
  (apply play-game (parse-input input)))
