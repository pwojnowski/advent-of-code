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

(defn- day-04-height? [data]
  (let [partition (- (count data) 2)
        num (.substring data 0 partition)
        unit (.substring data partition)]
    (when (and (re-matches #"^\d+$" num)
               (#{"cm" "in"} unit))
      (let [x (Integer/parseInt num)]
        (if (= unit "cm")
          (and (<= 150 x) (<= x 193))
          (and (<= 59 x) (<= x 76)))))))

(defn- number-in-range? [digits from to]
  (fn [data]
    (when (re-matches (re-pattern (str "^\\d{" digits "}$")) data)
      (let [year (Integer/parseInt data)]
        (and (<= from year) (<= year to))))))

(def day-04-passport-fields
  {
   "byr" (number-in-range? 4 1920 2002)
   "iyr" (number-in-range? 4 2010 2020)
   "eyr" (number-in-range? 4 2020 2030)
   "hgt" day-04-height?
   "hcl" #(re-matches #"^#[0-9a-f]{6}$" %)
   "ecl" #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"}
   "pid" #(re-matches #"^\d{9}$" %)
   })

(def day-04-required-passport-fields (set (keys day-04-passport-fields)))

(defn- has-required-keys? [found-keys]
  (every? (set found-keys) day-04-required-passport-fields))

(defn- passport-with-required-fields? [line]
  (let [found-keys (map #(.substring % 0 3) (s/split line #"\s"))]
    (has-required-keys? found-keys)))

(defn day-04-1-check-passports [input]
  (let [lines (s/split input #"\n\n+")]
    (count (filter passport-with-required-fields? lines))))

(defn- day-04-valid-field? [[k v]]
  ((get day-04-passport-fields k identity) v))

(defn- valid-passport? [line]
  (let [parts (s/split line #"\s")
        fields (into {} (map #(s/split % #":") parts))]
    (and (has-required-keys? (keys fields))
         (every? day-04-valid-field? fields))))

(defn day-04-2-check-passports [input]
  (let [lines (s/split input #"\n\n+")]
    (count (filter valid-passport? lines))))
