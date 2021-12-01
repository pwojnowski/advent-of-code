(ns aoc.utils
  (:require [clojure.string :as s]))

(defn load-file [filepath]
  (let [abs-path (s/replace-first filepath "~" (System/getProperty "user.home"))]
    (slurp abs-path)))

(defn load-lines [filepath]
  (s/split-lines (load-file filepath)))

(defn read-numbers
  "Parse `input' into a vector of numbers."
  [input]
  (mapv #(Long/parseLong %) (s/split-lines input)))
