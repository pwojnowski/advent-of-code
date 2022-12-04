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

(defn- day-03-get-common-item [rucksacks]
  (->> (map set rucksacks)
       (apply clojure.set/intersection)
       (first)))

(defn- day-03-item-priority [rucksacks]
  (day-03->priority (day-03-get-common-item rucksacks)))

(defn- day-03 [lines group-fn]
  (->> (group-fn lines)
       (map day-03-item-priority)
       (reduce + 0)))

(defn- day-03-1-create-groups [lines]
  (map #(split-at (/ (count %) 2) %) lines))

(defn day-03-1 [lines]
  (day-03 lines day-03-1-create-groups))

(defn- day-03-2-create-group [lines]
  (partition 3 lines))

(defn day-03-2 [input]
  (day-03 input day-03-2-create-group))

(defn- day-04->numeric-range [s]
  (->> (cstr/split s #",")
       (map #(cstr/split % #"-"))
       (map (fn [[x y]] (vector (parse-long x) (parse-long y))))))

(defn- day-04-fully-contained? [[[a b] [c d]]]
  (or (= a c) (= b d) ; if ends are the same then always one range contain another
      (and (< a c) (> b d))
      (and (> a c) (< b d))))

(defn day-04 [lines matching-fn]
  (->> (map day-04->numeric-range lines)
       (map matching-fn)
       (filter true?)
       (count)))

(defn day-04-1 [lines]
  (day-04 lines day-04-fully-contained?))

(defn- day-04-overlap? [[[a b] [c d]]]
  (not (or (< b c) (< d a))))

(defn day-04-2 [lines]
  (day-04 lines day-04-overlap?))
