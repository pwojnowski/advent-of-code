(ns aoc.utils
  (:require [clojure.string :as s]))

(defn read-file [filepath]
  (let [abs-path (s/replace-first filepath "~" (System/getProperty "user.home"))]
    (slurp abs-path)))

(defn read-lines [filepath]
  (s/split-lines (read-file filepath)))

(defn read-numbers
  "Parse `input' into a vector of numbers."
  [input]
  (mapv #(Long/parseLong %) (s/split-lines input)))
