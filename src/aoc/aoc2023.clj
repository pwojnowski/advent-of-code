(ns aoc.aoc2023
  (:require [clojure.string :as cstr]
            [aoc.utils :refer [->numbers]]))

;;; Day 1
(defn day-01 [input preprocessor]
  (->> (cstr/split input #"\n")
       (map preprocessor)
       (map #(vector (first %) (last %)))
       (map cstr/join)
       (map parse-long)
       (apply +)))

(defn day-01-1 [input]
  (day-01 input #(re-seq #"\d" %)))

(def word-digits {"one" "1" "two" "2" "three" "3" "four" "4" "five" "5"
                  "six" "6" "seven" "7" "eight" "8" "nine" "9"})

(defn- find-digits [s]
  (->> (re-seq #"(?=(one|two|three|four|five|six|seven|eight|nine|\d))" s)
       (map second)
       (map #(get word-digits % %))))

(defn day-01-2 [input]
  (day-01 input find-digits))
