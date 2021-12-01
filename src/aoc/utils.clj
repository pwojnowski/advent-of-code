(ns aoc.utils)

(defn load-lines [filepath]
  (clojure.string/split-lines (slurp filepath)))

(defn read-numbers
  "Parse `input' into a vector of numbers."
  [input]
  (mapv #(Long/parseLong %) (s/split-lines input)))
