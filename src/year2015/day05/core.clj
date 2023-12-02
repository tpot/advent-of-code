(ns year2015.day05.core
  (:require [clojure.string :as s]
            [clojure.test :refer [deftest is]]))

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

(defn is-nice?
  "Return true if string satifies all conditions for being nice."
  [x]
  (and (three-vowels? x)
       (doubles? x)
       (not (contains-naughty-substrings? x))))

(deftest test-is-nice?
  (let [test-input {"ugknbfddgicrmopn" true
                    "aaa"              true
                    "jchzalrnumimnmhp" false
                    "haegwjzuvuyypxyu" false
                    "dvszwmarrgswjxmb" false}]
    (is (= (map is-nice? (keys test-input))
           (vals test-input)))))

;; Part 1

(comment

  (def puzzle-input (-> (slurp "resources/year2015/day05/input") s/split-lines))

  (->> puzzle-input
       (map is-nice?)
       (filter true?)
       count) ;; => 255

  :rcf)