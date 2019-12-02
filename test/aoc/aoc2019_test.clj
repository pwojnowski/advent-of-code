(ns aoc.aoc2019-test
  (:require [aoc.aoc2019 :as aoc]
            [clojure.test :refer :all]))

(deftest test-day-01-part1
  (let [masses [12 14 1969 100756]
        expected-fuel (+ 2 2 654 33583)]
    (is (= expected-fuel (aoc/day01-part1 masses)))))
