(ns aoc.aoc2015)

(defn day-01-what-floor [input]
  (reduce #(+ % (if (= %2 \() 1 -1)) 0 input))
