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
  (is (= nil (part01 puzzle-input))))

(defn part02
  [input])

(deftest test-part02)

(run-tests)
