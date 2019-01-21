(ns aoc.aoc2018)

;;; Day 02 - part 1:
(defn only-twos-and-threes [numbers]
  (filter #(or (= 2 %1) (= 3 %1)) numbers))

(defn day02-part1 [data]
  (->> (map frequencies data)
       (map vals)
       (map distinct)
       (map only-twos-and-threes)
       (flatten)
       (frequencies)
       (vals)
       (apply *)))

;;; Day 2 - part 2:
(defn hamming-distance
  "Calculate Hamming distance between two strings."
  [s1 s2]
  (apply + (map #(if (= %1 %2) 0 1) s1 s2)))

(defn fabric-boxes? [id1 id2]
  (= (hamming-distance id1 id2) 1))

(defn common-chars [s1 s2]
  (clojure.string/join (map #(when (= %1 %2) %1) s1 s2)))

(defn find-common-chars [id1 id2]
  (when (fabric-boxes? id1 id2)
    (reduced (common-chars id1 id2))))

(defn try-match [id boxes]
  (reduce #(find-common-chars id %2) id boxes))

(defn scan-boxes [[id & boxes]]
  (when (seq boxes)
    (or (try-match id boxes) (recur boxes))))

;;; Day 3

;; A claim like #123 @ 3,2: 5x4 means that claim ID 123 specifies
;; a rectangle 3 inches from the left edge, 2 inches from the top
;; edge, 5 inches wide, and 4 inches tall.
(def claim-pattern
  (re-pattern "^#\\d+ @ (\\d+),(\\d+): (\\d+)x(\\d+)$"))

(def fabric (atom {}))

(defn parse-claim [claim]
  (->> (re-matcher claim-pattern claim)
       (re-find)
       (rest)
       (mapv read-string)))

(defn mark-inch [position]
  (swap! fabric update position (fnil inc 0)))

(defn mark-on-fabric [[left top width height]]
  (for [y (range top (+ top height))
        x (range left (+ left width))]
    (mark-inch [x y])))

(defn count-overlapping-inches []
  (->> (vals @fabric)
       (filter #(< 1 %))
       (count)))

(defn solve-day03 [claims]
  (let [fabric (atom {})]
    (->> claims
         (map parse-claim)
         (map #(mark-on-fabric fabric %))
         (count-overlapping-inches))
    ;;(println (count (vals @fabric)))
    ))
