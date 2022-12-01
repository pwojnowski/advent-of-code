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
