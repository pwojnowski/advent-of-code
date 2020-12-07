(ns aoc.aoc2020
  (:require [clojure.string :as s]))

;;; Day 1
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

;;; Day 2
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

;;; Day 3
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

;;; Day 4
(defn- day-04-height? [data]
  (let [partition (- (count data) 2)
        num (subs data 0 partition)
        unit (subs data partition)]
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
  (let [found-keys (map #(subs % 0 3) (s/split line #"\s"))]
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

;;; Day 5
(defn- day-05-find-index [code from size]
  (if (zero? size)
    (dec from)
    (let [mid (quot size 2)]
      (if (contains? #{\F \L} (first code))
        (recur (rest code) from mid)
        (recur (rest code) (+ from mid) mid)))))

(defn- day-05-decode-seat-id [code]
  (let [[row-code col-code] (split-at 7 code)]
    (+ (* 8 (day-05-find-index row-code 1 128))
       (day-05-find-index col-code 1 8))))

(defn day-05-max-seat-id [input]
  (let [codes (s/split input #"\n")]
    (apply max (map day-05-decode-seat-id codes))))

(defn day-05-find-my-seat-id [input]
  (let [codes (s/split input #"\n")
        sorted (sort (map day-05-decode-seat-id codes))
        first-id (first sorted)
        last-id (last sorted)]
    (->> (map #(when (not= %1 %2) %1) sorted (range first-id last-id))
         (drop-while nil?)
         (first)
         (dec))))

;;; Day 6
(defn- day-06-declarations [input counting-fn]
  (let [groups (s/split input #"\n\n+")]
    (apply + (map counting-fn groups))))

(defn- day-06-count-any-yes [group]
  (count (set (s/join (s/split group #"\s")))))

(defn day-06-1-declarations [input counting-fn]
  (day-06-declarations input day-06-count-any-yes))

(defn- day-06-count-every-yes [group]
  (->> (map set (s/split group #"\s"))
       (apply clojure.set/intersection)
       (count)))

(defn day-06-2-declarations [input]
  (day-06-declarations input day-06-count-every-yes))

;;; Day 7
(defn- parse-bag-name [content]
  (let [[_ p1 p2] (.split content " ")]
    (str p1 " " p2)))

(defn- add-bag-to-tree [tree parent bag-desc]
  (let [bag-name (parse-bag-name bag-desc)]
    (update tree bag-name #(if (nil? %) #{%2} (conj % %2)) parent)))

(defn- add-line-to-tree [tree line]
  (let [[parent-bag children-data] (s/split line #" bags contain ")]
    (if (= "no other bags." children-data)
      tree
      (reduce #(add-bag-to-tree % parent-bag %2) tree (s/split children-data #", ")))))

(defn- day-07-1-find-bags [tree bag-name]
  (when-let [children (get tree bag-name)]
    (concat children (mapcat #(day-07-find-bags tree %) children))))

(defn day-07-1-count-bags [input bag-name]
  (let [lines (s/split-lines input)
        tree (reduce #(add-line-to-tree % %2) {} lines)]
    (count (set (day-07-find-bags tree bag-name)))))
