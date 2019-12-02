(ns aoc.aoc2019)

(defn- day01-calculate-fuel [mass]
  (- (quot mass 3) 2))

(defn day01-part1 [masses]
  (->> masses
       (map day01-calculate-fuel)
       (reduce + 0)))
