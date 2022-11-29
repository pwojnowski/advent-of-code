(ns aoc.aoc2021
  (:require [clojure.string :as cstr]
            [aoc.utils :refer [->numbers]]))

(defn day-01-1 [input]
  (let [numbers (->numbers input)
        len (count numbers)]
    (loop [prev (first numbers) xs (rest numbers) i 1 cnt 0]
      (if (= i len)
        cnt
        (recur (first xs) (rest xs) (inc i)
               (if (< prev (first xs)) (inc cnt) cnt))))))

(defn day-01-2 [input]
  (let [numbers (->numbers input)
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

(defn- day-02-read-commands [input]
  (->> (cstr/split-lines input)
       (map #(cstr/split % #" "))
       (map (fn [[cmd v]] [cmd (Long/parseLong v)]))))

(defn- day-02-sum-values [commands]
  (reduce #(update % (first %2) (fnil + 0) (second %2)) {} commands))

(defn- day-02-calculate-horiz-pos [sums]
  (* (get sums "forward")
     (- (get sums "down") (get sums "up"))))

(defn day-02-1 [input]
  (-> (day-02-read-commands input)
      (day-02-sum-values)
      (day-02-calculate-horiz-pos)))

(defn day-02-2 [input]
  (loop [horiz 0 depth 0 aim 0 cmds (cstr/split-lines input)]
    (if (seq cmds)
      (let [[cmd s] (cstr/split (first cmds) #" ")
            val (Long/parseLong s)]
        (condp = cmd
          "forward" (recur (+ horiz val)
                           (+ depth (* aim val))
                           aim
                           (rest cmds))
          "up" (recur horiz depth (- aim val) (rest cmds))
          "down" (recur horiz depth (+ aim val) (rest cmds))))
      (* horiz depth))))

(defn- str->binvec [s]
  (mapv #(- (int %) 48) s))

(defn- zipsum [vectors]
  (apply (partial map +) vectors))

(defn- calculate-gamma [numbers]
  (let [half (/ (count numbers) 2)]
    (map #(if (pos? (- % half)) 1 0)
         (zipsum numbers))))

(defn- calculate-epsilon [gamma-vector]
  (map #(if (zero? %) 1 0) gamma-vector))

(defn day-03-1 [input]
  (let [numbers (map str->binvec (cstr/split-lines input))
        gv (calculate-gamma numbers)
        ev (calculate-epsilon gv)]
    (* (Long/parseLong (cstr/join gv) 2)
       (Long/parseLong (cstr/join ev) 2))))
