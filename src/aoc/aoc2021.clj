(ns aoc.aoc2021
  (:require [clojure.string :as s]
            [clojure.set]
            [aoc.utils :refer [read-numbers]]))

(defn day-01-1 [input]
  (let [numbers (read-numbers input)
        len (count numbers)]
    (loop [prev (first numbers) xs (rest numbers) i 1 cnt 0]
      (if (= i len)
        cnt
        (recur (first xs) (rest xs) (inc i)
               (if (< prev (first xs)) (inc cnt) cnt))))))

(defn day-01-2 [input]
  (let [numbers (read-numbers input)
        len (count numbers)]
    (loop [psum (reduce '+ (take 3 numbers))
           i 3
           cnt 0]
      (if (= i len)
        cnt
        (let [sum (+ (- psum (get numbers (- i 3)))
                     (get numbers i))]
          (recur sum
                 (inc i)
                 (if (< psum sum) (inc cnt) cnt)))))))
