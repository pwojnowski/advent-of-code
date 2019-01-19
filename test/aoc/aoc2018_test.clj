(ns aoc.aoc2018-test
  (:require [clojure.test :refer :all]
            [aoc.aoc2018 :refer :all]))

(deftest test-day02
  (testing "Solution to Day 2 part 1"
    (let [data ["abcdef" "bababc" "abbcde" "abcccd" "aabcdd" "abcdee" "ababab"]]
      (is (= (day02-part1 data) 12))))

  (testing "Solution to Day 2 part 2"
    (let [data ["abcde" "fghij" "klmno" "pqrst" "fguij" "axcye" "wvxyz"]]
      (is (= (scan-boxes data) "fgij")))))
