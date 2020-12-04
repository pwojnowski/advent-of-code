(ns aoc.aoc2020
  (:require [clojure.string :as s]))

(defn day-01-1 [lines]
  (let [nums #{} (map #(Integer/parseInt %) lines)
        diffs (into {} (map #(vector (- 2020 %) %) nums))
        x (some diffs nums)]
    (* x (diffs x))))

(defn day-01-2 [lines]
  (let [nums #{} (map #(Integer/parseInt %) lines)]
    (first
     (for [a nums b nums :when (nums (- 2020 a b))]
       (* a b (- 2020 a b))))))

(defn- parse-password-policy [policy]
  (let [[nums charStr] (s/split policy #" ")
        letter (first charStr)
        [a b] (map #(Integer/parseInt %) (s/split nums #"-"))]
    [a b letter]))

(defn- old-policy-validator [a b letter password]
  (let [n (get (frequencies password) letter 0)]
    (and (<= a n) (<= n b))))

(defn- new-policy-validator [a b letter password]
  (let [al (get password (dec a))
        bl (get password (dec b))]
    (and (not= al bl)
         (or (= al letter) (= bl letter)))))

(defn- is-valid-password? [validator-fn line]
  (let [[policy password] (s/split line #": ")
        [a b letter] (parse-password-policy policy)]
    (validator-fn a b letter password)))

(defn day-02-1-valid-passwords [lines]
  (count (filter #(is-valid-password? old-policy-validator %) lines)))

(defn day-02-2-valid-passwords [lines]
  (count (filter #(is-valid-password? new-policy-validator %) lines)))

(defn day-03-count-trees [lines x]
  (let [ys-range (range (count lines))
        len (count (first lines))]
    (->> (mapv (fn [y] (get (lines y) (rem (* x y) len))) ys-range)
         (filter #(= \# %))
         (count))))

;; For changes: x+1, y+2
;; (let [len (count (first lines))
;;       ys-range (range 0 (count lines) 2)]
;;   (->> (mapv #(get (get lines %1) (rem %2 len)) ys-range (range))
;;        (filter #(= \# %))
;;        (count)))
