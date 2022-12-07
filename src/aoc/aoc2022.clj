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
      (repeat (list))
      (vec)))

(def ^:const day-05-crate-distance 4)

(defn- day-05-crate-positions [n-stacks]
  (range 1
         (- (* n-stacks day-05-crate-distance) 2)
         day-05-crate-distance))

(defn- day-05->crates [positions line]
  (->> (map #(get line %) positions)
       (map-indexed #(when-not (= \space %2) (vector %1 %2)))
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
  (let [[n from to] (map parse-long (re-seq #"\d+" line))]
    [n (dec from) (dec to)]))

(defn- day-05-pop-crate [stacks idx]
  [(update-in stacks [idx] pop)
   (peek (get stacks idx))])

(defn- day-05-move-crate [stacks from to]
  (let [[stacks crate] (day-05-pop-crate stacks from)]
    (day-05-push-crate stacks [to crate])))

(defn day-05 [input move-applicator]
  (let [[stacks-def moves-def] (map cstr/split-lines (cstr/split input #"\n\n"))
        stacks (day-05->stacks stacks-def)]
    (->> (map day-05->move moves-def)
         (reduce move-applicator stacks)
         (map first)
         (apply str))))

(defn- day-05-1-apply-moves [stacks [n from to]]
  (loop [i 0 stacks stacks]
    (if (< i n)
      (recur (inc i) (day-05-move-crate stacks from to))
      stacks)))

(defn day-05-1 [input]
  (day-05 input day-05-1-apply-moves))

(defn- day-05-2-apply-move [stacks [n from to]]
  (let [crates (take n (get stacks from))]
    (update-in (update-in stacks [to] #(concat crates %))
               [from] #(drop n %))))

(defn day-05-2 [input]
  (day-05 input day-05-2-apply-move))

(defn day-06 [input n]
  (loop [seen (apply conj (clojure.lang.PersistentQueue/EMPTY) (take n input)) i n]
    (if (= (count (set seen)) n)
      i
      (recur (conj (pop seen) (get input i)) (inc i)))))

(defn day-06-1 [input]
  (day-06 input 4))

(defn day-06-2 [input]
  (day-06 input 14))

(defn- day-07->node
  ([name size]
   {:name name :type :file :size size})
  ([name size children]
   {:name name :type :dir :size size :children children}))

(defn- day-07->file [line]
  (let [[size name] (cstr/split line #" ")]
    (day-07->node name (parse-long size))))

(defn- day-07-load-children [lines]
  (loop [lines lines children []]
    (let [line (first lines)]
      (cond
        (nil? line) [lines children]
        (cstr/starts-with? line "$ ls") (recur (rest lines) children)
        (cstr/starts-with? line "dir ") (recur (rest lines) children)
        (cstr/starts-with? line "$ cd ..") [(rest lines) children]
        (cstr/starts-with? line "$ cd") (let [[lines dir] (day-07-load-dir lines line)]
                                          (recur lines (conj children dir)))
        :else (recur (rest lines) (conj children (day-07->file line)))))))

(defn- day-07-load-dir [lines line]
  (let [name (nth (cstr/split line #" ") 2)
        [lines children] (day-07-load-children (rest lines))
        size (apply + (map :size children))]
    [lines (day-07->node name size children)]))

(defn- day-07-get-dir-sizes [fs]
  (->> (tree-seq :children :children fs)
       (filter #(= (:type %) :dir))
       (map :size)))

(defn- day-07-total-size [fs]
  (->> (day-07-get-dir-sizes fs)
       (filter #(<= % 100000))
       (reduce + 0)))

(defn day-07-1 [lines]
  (let [fs (second (day-07-load-dir lines (first lines)))]
    (day-07-total-size fs)))

(def ^:const total-disk-space 70000000)
(def ^:const min-required-free-space 30000000)

(defn day-07-2 [lines]
  (let [root (second (day-07-load-dir lines (first lines)))
        used-space (:size root)
        free-space (- total-disk-space used-space)
        missing (- min-required-free-space free-space)]
    (->> (day-07-get-dir-sizes root)
         (filter #(<= missing %))
         (sort)
         (first))))
