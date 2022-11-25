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

(defn- day-02-get-box-sizes [s]
  (map parse-long (clojure.string/split s #"x")))

(defn- day-02-calculate-required-paper [s]
  (let [[l w h] (day-02-get-box-sizes s)
        lw (* l w)
        lh (* l h)
        wh (* w h)
        smallest-side (min lw lh wh)]
    (+ (* 2 (+ lw lh wh)) smallest-side)))

(defn day-02-part-1 [input]
  (reduce + 0 (map day-02-calculate-required-paper input)))

(defn- day-02-calculate-required-ribbon [s]
  (let [[l w h] (day-02-get-box-sizes s)
        biggest (* 2 (max l w h))
        perimeter (- (* 2 (+ l w h)) biggest)
        bow-ribbon (* l w h)]
    (+ perimeter bow-ribbon)))

(defn day-02-part-2 [input]
  (reduce + 0 (map day-02-calculate-required-ribbon input)))
