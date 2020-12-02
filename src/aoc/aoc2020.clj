(ns aoc.aoc2020)

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
  (let [[nums charStr] (clojure.string/split policy #" ")
        letter (first charStr)
        [a b] (map #(Integer/parseInt %) (clojure.string/split nums #"-"))]
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
  (let [[policy password] (clojure.string/split line #": ")
        [a b letter] (parse-password-policy policy)]
    (validator-fn a b letter password)))

(defn day-02-1-valid-passwords [lines]
  (count (filter #(is-valid-password? old-policy-validator %) lines)))

(defn day-02-2-valid-passwords [lines]
  (count (filter #(is-valid-password? new-policy-validator %) lines)))
