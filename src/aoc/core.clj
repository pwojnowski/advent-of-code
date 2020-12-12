(ns aoc.core
  (:require [aoc.aoc2020 :as aoc20]))

(defn -main [& args]
  (println "Starting the app...")
  (time (aoc20/day-10-brute-arrangements (slurp "../aoc2020-10-input.txt"))))
