(ns year2015.day05.core
  (:require [clojure.string :as s]
            [clojure.test :refer [deftest is]]))

(def puzzle-input (-> (slurp "resources/year2015/day05/input") s/split-lines))

;; Part 1

(defn three-vowels?
  "Return true if string contains at least three vowels."
  [x]
  (>= (->> (map (comp re-pattern str) "aeiou")
           (map #(re-seq % x))
           (map count)
           (apply +))
      3))

(deftest test-three-vowels?
  (is (not (three-vowels? "abc")))
  (is (three-vowels? "aaa"))
  (is (three-vowels? "aeiou")))

(defn doubles?
  "Return true is string contains at least one letter twice in a row."
  [x]
  (reduce
   (fn [_ [a b]] (if (= a b) (reduced true) false))
   false
   (partition 2 1 x)))

(deftest test-doubles?
  (is (not (doubles? "abc")))
  (is (doubles? "abba")))

(defn contains-naughty-substrings?
  "Return true if string contains any naughty substrings."
  [x]
  (->> ["ab" "cd" "pq" "xy"]
       (map (partial s/index-of x))
       (remove nil?)
       count
       (not= 0)))

(deftest test-contains-naughty-substrings?
  (is (contains-naughty-substrings? "haegwjzuvuyypxyu"))
  (is (not (contains-naughty-substrings? "qqqq"))))

(defn is-nice-part1?
  "Return true if string satifies all conditions for being nice."
  [x]
  (and (three-vowels? x)
       (doubles? x)
       (not (contains-naughty-substrings? x))))

(deftest test-is-nice-part1?
  (let [test-input {"ugknbfddgicrmopn" true
                    "aaa"              true
                    "jchzalrnumimnmhp" false
                    "haegwjzuvuyypxyu" false
                    "dvszwmarrgswjxmb" false}]
    (is (= (map is-nice-part1? (keys test-input))
           (vals test-input)))))

(comment

  (->> puzzle-input
       (map is-nice-part1?)
       (filter true?)
       count) ;; => 255

  :rcf)

;; Part 2

(defn contains-nonoverlapping-pair?
  "Does the string contain a pair of characters that do not overlap?"
  [s]
  (let [pairs         (partition 2 1 s)
        indexed-pairs (map-indexed #(hash-map :index %1 :pair %2) pairs)
        grouped-pairs (group-by :pair indexed-pairs)]
    (reduce
     (fn [_ input]
       (let [indexes (map :index input)]
         (if (->> (for [i indexes j indexes] (vector i j))
                  (map #(abs (- (first %) (second %))))
                  (some #(> % 1)))
           (reduced true)
           false)))
     false
     (vals grouped-pairs))))

(deftest test-contains-nonoverlapping-pair?
  (is (contains-nonoverlapping-pair? "xxyxx"))
  (is (not (contains-nonoverlapping-pair? "aaa")))
  (is (contains-nonoverlapping-pair? "aaaa")))

(defn contains-duplicate-with-inbetween?
  [s]
  (->> (partition 3 1 s)
       (some (fn [[a _ c]] (= a c)))))

(defn is-nice-part2?
  [s]
  (and (contains-nonoverlapping-pair? s)
       (contains-duplicate-with-inbetween? s)))

(comment

  (->> puzzle-input
       (map is-nice-part2?)
       (filter true?)
       count) ;; => 55

  :rcf)
