(ns year2024.day02.core
  (:require [clojure.string :as s]
            [clojure.test :refer [deftest is run-tests]]))

(def test-input "resources/year2024/day02/test-input")
(def puzzle-input "resources/year2024/day02/input")

(defn safe?
  "Return true if a report is safe"
  [report]
  (or
   (= (->> (partition 2 1 report)
           (map #(- (second %) (first %)))
           (filter #(and (>= % 1) (<= % 3)))
           count)
      (dec (count report)))
   (= (->> (partition 2 1 report)
           (map #(- (first %) (second %)))
           (filter #(and (>= % 1) (<= % 3)))
           count)
      (dec (count report)))))

(defn parse-input
  [input]
  (->> (-> (slurp input) s/split-lines)
       (map #(s/split % #" "))
       (map #(map parse-long %))))

(defn part01
  [input]
  (->> (parse-input input)
       (filter safe?)
       count))

(deftest test-part01
  (is (= 2 (part01 test-input)))
  (is (= 236 (part01 puzzle-input))))

(defn drop-one
  "Return a seq of reports where one element is dropped"
  [report]
  (for [n (range (count report))]
    (concat (take n report)
            (rest (drop n report)))))

(defn safe-with-damper?
  [report]
  (if (safe? report) true
      (> (->> (drop-one report)
              (filter safe?)
              count)
         0)))

(deftest test-safe-with-damper?
  (is (true?  (safe-with-damper? [7 6 4 2 1])))
  (is (false? (safe-with-damper? [1 2 7 8 9])))
  (is (false? (safe-with-damper? [9 7 6 2 1])))
  (is (true?  (safe-with-damper? [1 3 2 4 5])))
  (is (true?  (safe-with-damper? [8 6 4 4 1])))
  (is (true?  (safe-with-damper? [1 3 6 7 9]))))

(defn part02
  [input]
  (->> (parse-input input)
       (filter safe-with-damper?)
       count))

(deftest test-part02
  (is (= 4 (part02 test-input)))
  (is (= 308 (part02 puzzle-input))))

(run-tests)
