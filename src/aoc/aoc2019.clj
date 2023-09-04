(ns aoc.aoc2019)

(defn- day01-calculate-fuel [mass]
  (- (quot mass 3) 2))

(defn day01-part1 [masses]
  (->> masses
       (map day01-calculate-fuel)
       (reduce + 0)))

(defn- day01-calculate-non-negative-fuel [mass]
  (let [fuel (day01-calculate-fuel mass)]
    (max 0 fuel)))

(defn- day01-calculate-total-fuel [mass]
  (->> (day01-calculate-non-negative-fuel mass)
       (iterate day01-calculate-non-negative-fuel)
       (take-while pos?)
       (apply +)))

(defn day01-part2 [masses]
  (reduce + 0 (map day01-calculate-total-fuel masses)))
