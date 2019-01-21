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

(deftest test-day03
  (testing "Solution to Day 3 part 1"
    (let [claims ["#1 @ 1,3: 4x4" "#2 @ 3,1: 4x4" "#3 @ 5,5: 2x2"]]
      (is (= (solve-day03 claims) 4))))
  (testing "Parsing claim input data."
    (is (= [3 2 5 4] (parse-claim "#123 @ 3,2: 5x4")))
    (is (= [126 902 29 28] (parse-claim "#1 @ 126,902: 29x28"))))
  (testing "Claim should be translated into coordinates of inches to mark."
    (is (= (inches-to-mark [0 0 0 0]) []))
    (is (= (inches-to-mark [3 2 5 4])
           [[3 2] [4 2] [5 2] [6 2] [7 2]
            [3 3] [4 3] [5 3] [6 3] [7 3]
            [3 4] [4 4] [5 4] [6 4] [7 4]
            [3 5] [4 5] [5 5] [6 5] [7 5]]))
    (is (= (* 29 28) (count (inches-to-mark [126 902 29 28]))))))

(deftest mark-inches-on-fabric-test
  (testing "Mark claim inches on fabric."
    (let [fabric (atom {})
          positions (sort (inches-to-mark [3 2 5 4]))]
      (mark-claim fabric positions)
      (is (= (sort (keys @fabric)) positions))
      (is (= (vals @fabric) (repeat 20 1)))))

  (testing "Should inc value on overlapping claims."
    (let [fabric (atom {})]
      (mark-claim fabric (inches-to-mark [1 1 1 1]))
      (mark-claim fabric (inches-to-mark [1 1 1 1]))
      (is (= (keys @fabric) [[1 1]]))
      (is (= (vals @fabric) [2])))))

(defn- find-positions [parsed-input]
  (sort (mapcat inches-to-mark parsed-input)))

(defn- count-marked-inches [parsed-input]
  (->> parsed-input
       (map #(* (nth % 2) (nth % 3)))
       (apply +)))

(deftest mark-inches-on-fabric-test
  (testing "Check consistency on real data."
    ;; 1. Number of marks should be the same as the number of inches.
    ;; 2. Sum of all marked inches (with overlaps) should match
    ;; total number of inches passed for marking.
    (let [input (load-input "/Users/przemek/poligon/clojure/day3-input.txt")
          parsed-input (take 1 (map parse-claim input))
          fabric (atom {})]
      (mark-claims fabric (take 1 input))
      (is (= (sort (keys @fabric)) (find-positions parsed-input)))
      ;; (is (= (reduce + 0 (vals @fabric)) (count-marked-inches parsed-input)))
      )))
