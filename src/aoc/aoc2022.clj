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

(defn- day-05->empty-stacks [header]
  (-> (cstr/trim header)
      (cstr/split  #"\s+")
      (count)
      (repeat [])
      (vec)))

(def ^:const day-05-crate-distance 4)

(defn- day-05-crate-positions [n-stacks]
  (range 1
         (- (* n-stacks day-05-crate-distance) 2)
         day-05-crate-distance))

(defn- day-05->crates [positions line]
  (->> (map #(get line %) positions)
       (map-indexed #(if (= \space %2) nil (vector %1 %2)))
       (filter some?)))

(defn- day-05-push-crate [stacks [stack-idx crate]]
  (update-in stacks [stack-idx] conj crate))

(defn- day-05-fill-stacks [stacks crates]
  (if-let [row (first crates)]
    (recur (reduce #(day-05-push-crate %1 %2) stacks row)
           (rest crates))
    stacks))

(defn- day-05->stacks [stack-lines]
  (let [stack-lines (reverse stack-lines)
        stacks (day-05->empty-stacks (first stack-lines))
        n-stacks (count stacks)
        crate-positions (day-05-crate-positions n-stacks)]
    (->> (rest stack-lines)
         (map #(day-05->crates crate-positions %))
         (day-05-fill-stacks stacks))))

(defn- day-05->move [line]
  (->> (re-seq #"\d+" line)
       (mapv (comp dec parse-long))))

(defn- day-05-pop-crate [stacks idx]
  [(update-in stacks [idx] pop)
   (peek (get stacks idx))])

(defn- day-05-move-crate [stacks from to]
  (let [[stacks crate] (day-05-pop-crate stacks from)]
    (day-05-push-crate stacks [to crate])))

(defn- day-05-apply-moves [stacks [n from to]]
  (loop [i 0 stacks stacks]
    (if (<= i n)
      (recur (inc i) (day-05-move-crate stacks from to))
      stacks)))

(defn day-05-1 [input]
  (let [[stacks-def moves-def] (map cstr/split-lines (cstr/split input #"\n\n"))
        stacks (day-05->stacks stacks-def)]
    (->> (map day-05->move moves-def)
         (reduce day-05-apply-moves stacks)
         (map peek)
         (apply str))))
