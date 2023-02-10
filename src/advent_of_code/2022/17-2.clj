(ns advent-of-code.2022.17
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def heighest-points
  [0 0 0 0 0 0 0])

(def test-input ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")

(def blocks
  [1 1 1 1]
  [2 3 2]
  [1 1 3]
  [4]
  [2 2])
