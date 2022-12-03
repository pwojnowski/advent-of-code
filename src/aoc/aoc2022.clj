(ns aoc.aoc2022
  (:require [clojure.string :as cstr]
            [aoc.utils :refer [->numbers]]))

(defn day-01 [input n]
  (->> (cstr/split input #"\n\n")
       (map ->numbers)
       (mapv #(apply + %))
       (sort >)
       (take n)
       (apply +)))

(def day-02-1-choice-scores {\X 1 \Y 2 \Z 3})

(def day-02-1-score-table
  {"A X" 3, "A Y" 6, "A Z" 0,
   "B X" 0, "B Y" 3, "B Z" 6,
   "C X" 6, "C Y" 0, "C Z" 3})

(defn- day-02-1-score-round [s]
  (+ (day-02-1-choice-scores (last s))
     (day-02-1-score-table s)))

(defn day-02-1 [input]
  (apply + (map day-02-1-score-round input)))

(def day-02-2-score-result {\X 0 \Y 3 \Z 6})

(def day-02-2-choice-table
  {"A X" 3, "A Y" 1, "A Z" 2,
   "B X" 1, "B Y" 2, "B Z" 3
   "C X" 2, "C Y" 3, "C Z" 1})

(defn- day-02-2-score-round [s]
  (+ (day-02-2-score-result (last s))
     (day-02-2-choice-table s)))

(defn day-02-2 [input]
  (apply + (map day-02-2-score-round input)))


(defn- day-03-prioritize [from-char to-char idx-start]
  (into {}
        (map #(vector (char %1) %2)
             (range (int from-char) (inc (int to-char)))
             (iterate inc idx-start))))

(def day-03->priority
  (merge (day-03-prioritize \a \z 1)
         (day-03-prioritize \A \Z 27)))

(defn- day-03-get-common-item [s]
  (->> (split-at (/ (count s) 2) s)
       (map set)
       (apply clojure.set/intersection)
       (first)))

(defn- day-03-item-priority [s]
  (day-03->priority (day-03-get-common-item s)))

(defn day-03-1 [input]
  (reduce + 0 (map day-03-item-priority input)))
