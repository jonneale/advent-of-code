#!/bin/bash

YEAR=$1
DAY=$2

`curl https://adventofcode.com/${YEAR}/day/${DAY}/input --cookie "session=$SESSION" > ./resources/${YEAR}/${DAY}.txt`

`touch ./src/advent_of_code/${YEAR}/${DAY}.clj`
