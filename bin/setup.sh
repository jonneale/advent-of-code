#!/bin/bash
TODAY=`date +'%-d'`
THIS_YEAR=`date +'%Y'`

YEAR=${1:-$THIS_YEAR}
DAY=${2:-$TODAY}

`curl https://adventofcode.com/${YEAR}/day/${DAY}/input --cookie "session=$SESSION" > ./resources/${YEAR}/${DAY}.txt`

`echo "(ns advent-of-code.${YEAR}.${DAY}
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))
(def input
    (->> (-> (io/resource \"${YEAR}/${DAY}.txt\")
              slurp
             (clojure.string/split  #\"\\n\"))
       (map #(clojure.string/split % #\"\\,\"))))" > ./src/advent_of_code/${YEAR}/${DAY}.clj`

