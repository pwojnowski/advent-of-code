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

(defn- day-02-calculate-required-paper [box-sizes]
  (let [[l w h] (map parse-long (clojure.string/split box-sizes #"x"))
        lw (* l w)
        lh (* l h)
        wh (* w h)
        smallest-side (min lw lh wh)]
    (+ (* 2 (+ lw lh wh)) smallest-side)))

(defn day-02-part-1 [input]
  (reduce + 0 (map day-02-calculate-required-paper input)))
