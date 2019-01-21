(ns aoc.utils)

(defn load-lines [filepath]
  (clojure.string/split-lines (slurp filepath)))
