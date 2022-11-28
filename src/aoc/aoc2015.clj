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

(defn- day-03-dir-to-point [[x y] dir]
  (condp = dir
    \^ [x (inc y)]
    \v [x (dec y)]
    \> [(inc x) y]
    \< [(dec x) y]))

(defn- day-03-find-visited-houses [dirs]
  (loop [houses #{[0 0]} pos [0 0] dirs dirs]
    (if-let [dir (first dirs)]
      (let [pos (day-03-dir-to-point pos dir)]
        (recur (conj houses pos) pos (rest dirs)))
      houses)))

(defn day-03-part-1 [dirs]
  (count (day-03-find-visited-houses dirs)))

(defn- day-03-separate-moves [dirs]
  (let [groups (group-by #(even? (first %)) (map-indexed #(vector % %2) dirs))
        santas (map second (get groups true))
        robots (map second (get groups false))]
    [santas robots]))

(defn day-03-part-2 [dirs]
  (->> (day-03-separate-moves dirs)
       (map day-03-find-visited-houses)
       (apply clojure.set/union)
       (count)))

(def day-04-md5-digester (java.security.MessageDigest/getInstance "MD5"))

(defn- day-04-get-md5 [^String s]
  (->> (.getBytes s)
       (.digest day-04-md5-digester)
       (BigInteger. 1)
       (format "%032x")))

(defn day-04-find-md5-with-prefix [^String input ^String prefix]
  (loop [n 1]
    (let [md5 (day-04-get-md5 (str input n))]
      (if (clojure.string/starts-with? md5 prefix)
        n
        (recur (inc n))))))

(defn day-04-part-1 [^String input]
  (day-04-find-md5-with-prefix input "00000"))

(defn day-04-part-2 [^String input]
  (day-04-find-md5-with-prefix input "000000"))
