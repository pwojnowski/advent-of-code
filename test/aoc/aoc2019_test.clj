(ns aoc.aoc2019-test
  (:require [aoc.aoc2019 :as aoc]
            [clojure.test :refer :all]))

(deftest test-day-01-part1
  (let [masses [12 14 1969 100756]
        expected-fuel (+ 2 2 654 33583)]
    (is (= expected-fuel (aoc/day01-part1 masses)))))

(deftest test-day-01-part2
  (testing "Fuel for single mass"
    (is (= 2 (aoc/day01-part2 [12])))
    (is (= 2 (aoc/day01-part2 [14])))
    (is (= 966 (aoc/day01-part2 [1969])))
    (is (= 50346 (aoc/day01-part2 [100756])))))
