(ns aoc.aoc2020
  (:require [clojure.string :as s]
            [clojure.set]
            [aoc.utils :refer [->numbers]]))

;;; Day 1
(defn day-01-1
  "Algo: x + y = 2020 -> y = 2020 - x.
  Create a map from y to x that produced it, then find any input number in
  that mapping - [y x]. Multiply to get the result."
  [input]
  (let [nums (->numbers input)
        diffs (into {} (map #(vector (- 2020 %) %) nums))
        x (some diffs nums)]
    (* x (diffs x))))

(comment ;; day-01-1 ver 2:
  (let [nums (set (->numbers input))]
    (->> (map #(vector (- 2020 %) %) nums)
         (some #(when (nums (first %)) %))
         (apply *))))

(defn day-01-2 [input]
  (let [nums (set (->numbers input))]
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
(defn- chars-on-my-way [lines [dy dx]]
  (let [len (count lines)
        line-len (count (first lines))]
    (for [i (range len) :when (< (* i dy) len)]
      (get (get lines (* i dy))
           (rem (* i dx) line-len)))))

(defn- count-trees [lines slope]
  (count (filter #(= \# %) (chars-on-my-way lines slope))))

(defn day-03-count-trees [input slopes]
  (let [lines (s/split-lines input)]
    (apply * (map #(count-trees lines %) slopes))))

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
  (let [[n p1 p2] (.split content " ")]
    [(str p1 " " p2) (Integer/parseInt n)]))

(defn- add-bag-name [tree parent bag-desc]
  (let [[bag-name] (parse-bag-name bag-desc)]
    (update tree bag-name #(if (nil? %) #{%2} (conj % %2)) parent)))

(defn- add-line-to-tree [build-fn tree line]
  (let [[parent-bag children-data] (s/split line #" bags contain ")]
    (if (= "no other bags." children-data)
      tree
      (reduce #(build-fn % parent-bag %2) tree (s/split children-data #", ")))))

(defn- day-07-find-bags [tree bag-name]
  (when-let [children (get tree bag-name)]
    (concat children (mapcat #(day-07-find-bags tree %) children))))

(defn day-07-1-count-outer-bags [input bag-name]
  (let [lines (s/split-lines input)
        tree (reduce #(add-line-to-tree add-bag-name % %2) {} lines)]
    (count (set (day-07-find-bags tree bag-name)))))

(defn- add-bag-count [tree parent bag-desc]
  (let [bag (parse-bag-name bag-desc)]
    (update tree parent #(if (nil? %) (conj {} %2) (conj % %2)) bag)))

(defn- day-07-2-count-bags [tree bag-name]
  (->> (get tree bag-name)
       (map (fn [[bn n]] (+ n (* n (day-07-2-count-bags tree bn)))))
       (apply +)))

(defn day-07-2-count-inner-bags [input bag-name]
  (let [lines (s/split-lines input)
        tree (reduce #(add-line-to-tree add-bag-count % %2) {} lines)]
    (day-07-2-count-bags tree bag-name)))


;;; Day 8
(defn- day-08-parse-instruction [data]
  (let [[op v] (s/split data #" ")]
    [op (Integer/parseInt v)]))

(defn day-08-ax-before-loop [input]
  (let [code (mapv day-08-parse-instruction (s/split-lines input))
        visited (boolean-array (count code))]
    (loop [ip 0 ax 0]
      (if (aget visited ip)
        ax
        (let [[op v] (get code ip)]
          (aset visited ip true)
          (condp = op
            "nop" (recur (inc ip) ax)
            "acc" (recur (inc ip) (+ ax v))
            "jmp" (recur (+ ip v) ax)))))))

(defn- day-08-terminates? [code]
  (let [max-ip (count code)
        visited (boolean-array max-ip)]
    (loop [ip 0 ax 0]
      (if (>= ip max-ip)
        ax
        (when-not (aget visited ip)
          (let [[op v] (get code ip)]
            (aset visited ip true)
            (condp = op
              "nop" (recur (inc ip) ax)
              "acc" (recur (inc ip) (+ ax v))
              "jmp" (recur (+ ip v) ax))))))))

(defn- nop-jmp-exchange [ip [op v]]
  (condp = op
    "nop" [ip "jmp" v]
    "jmp" [ip "nop" v]
    nil))

(defn- day-08-list-subs [code]
  (filter some? (map-indexed nop-jmp-exchange code)))

(defn- day-08-fixes? [code [ip op v]]
  (let [new-code (assoc code ip [op v])]
    (day-08-terminates? new-code)))

(defn day-08-ax-after-fix [input]
  (let [code (mapv day-08-parse-instruction (s/split-lines input))
        substs (day-08-list-subs code)]
    (->> (map #(day-08-fixes? code %) substs)
         (filter some?)
         (first))))

;;; Day 9
(defn- day-09-valid-number? [numbers i window-size]
  (let [x (get numbers i)
        parts (set (subvec numbers (- i window-size) i))]
    (some parts (map #(- x %) parts))))

(defn day-09-1-find-invalid-number [input window-size]
  (let [numbers (->numbers input)
        total (count numbers)]
    (loop [i window-size]
      (if (day-09-valid-number? numbers i window-size)
        (recur (inc i))
        (get numbers i)))))

(defn day-09-2-find-encryption-weakness [input target]
  (let [numbers (->numbers input)
        total (count numbers)]
    (loop [i 0 j 1 sum (+ (numbers i) (numbers j))]
      (cond
        (= sum target) (apply + (apply (juxt min max) (subvec numbers i j)))
        (< sum target) (recur i (inc j) (+ sum (numbers (inc j))))
        (> sum target) (recur (inc i) j (- sum (numbers i)))))))

;;; Day 10
(defn- find-joltage-diffs [[ones threes prev] x]
  (let [diff (- prev x)]
    (cond
      (= diff 1) [(inc ones) threes x]
      (= diff 3) [ones (inc threes) x]
      :else      [ones threes x])))

(defn day-10-joltage-diffs
  "Sort joltages and then walk through them counting how many diff by 1 or 3."
  [input]
  (let [numbers (reverse (sort (conj (->numbers input) 0)))
        device-joltage (+ (first numbers) 3)]
    (->> (reduce find-joltage-diffs [0 0 device-joltage] numbers)
         (take 2)
         (apply *))))

(defn- count-paths [cache graph i]
  (if-let [cached (@cache i)]
    cached
    (if-let [adjs (graph i)]
      (let [sum (apply + (map #(count-paths cache graph %) adjs))]
        (swap! cache assoc i sum)
        sum)
      1)))

(defn- find-matching-adapters [nums]
  (for [x nums y (rest nums) :when (#{1 2 3} (- x y))]
    [y x]))

(defn- build-arrangements-graph [nums]
  (reduce #(update % (first %2) conj (second %2))
          {}
          (find-matching-adapters nums)))

(defn day-10-dp-brute-arrangements
  "Sort joltages, then for each one find adapters on further than 3 from it.
  Build graph from all found connections and then walk it counting the number
  of possible paths from 0 to the device-joltage.
  A cache is critical optimization. Without it, a simple brute-force, will not
  finish in a reasonable time."
  [input]
  (let [nums (reverse (sort (conj (->numbers input) 0)))
        device-joltage (+ (first nums) 3)
        nums (conj nums device-joltage)
        cache (atom {})]
    (count-paths cache (build-arrangements-graph nums) 0)))


;;; Day 11
(defn- find-valid-positions
  [width height seat-x seat-y]
  (for [x (range (max 0 (dec seat-x)) (min width (+ seat-x 2)))
        y (range (max 0 (dec seat-y)) (min height (+ seat-y 2)))
        :when (or (not= x seat-x) (not= y seat-y))]
    [x y]))
