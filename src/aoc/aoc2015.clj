(ns aoc.aoc2015)

(defn day-01-what-floor [input]
  (reduce #(+ % (if (= %2 \() 1 -1)) 0 input))

(defn day-01-part-2 [input]
  (loop [i 0 level 0 c (first input) chars (rest input)]
    (if (= level -1)
      i
      (recur (inc i)
             (+ level (if (= c \() 1 -1))
             (first chars)
             (rest chars)))))
