(ns aoc.aoc2023
  (:require [clojure.string :as cstr]
            [aoc.utils :refer [->numbers]]))

;;; Day 1
(defn day-01 [input preprocessor]
  (->> (cstr/split-lines input)
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

;;; Day 2
(def cube-limits {"red" 12, "green" 13, "blue" 14})

(defn- day-02->cube-and-limits [game-line]
  (->> (re-seq #"(\d+) (blue|red|green)" game-line)
       (mapv #(vector (cube-limits (nth % 2)) (parse-long (second %))))))

(defn day-02-exceeds-limit? [[limit revealed]]
  (< limit revealed))

(defn- day-02->possible? [game-line]
  (not-any? day-02-exceeds-limit? (day-02->cube-and-limits game-line)))

(defn day-02->game-id [game-line]
  (parse-long (second (re-find #"Game (\d+)" game-line))))

(defn day-02-1 [input]
  (->> (cstr/split-lines input)
       (filter day-02->possible?)
       (mapv day-02->game-id)
       (apply +)))
