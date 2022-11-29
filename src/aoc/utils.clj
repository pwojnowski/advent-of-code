(ns aoc.utils
  (:require [clojure.string :as s]))

(defn read-file
  "Reads the whole file from given `filepath` as string.

  Example: (clojure.string/split-lines (read-file \"~/aoc/2015-01.txt\"))"
  [filepath]
  (let [abs-path (s/replace-first filepath "~" (System/getProperty "user.home"))]
    (slurp abs-path)))

(defn read-lines
  "Reads the whole file from given `filepath` and splits into lines.

  Example: (read-lines \"~/aoc/2015-01.txt\")"
  [filepath]
  (s/split-lines (read-file filepath)))

(defn ->numbers
  "Parse `input` into a vector of numbers.

  Example: (->numbers \"1\\n2\") ;; [1 2]"
  [input]
  (mapv parse-long (s/split-lines input)))
