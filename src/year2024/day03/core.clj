(ns year2024.day03.core
  (:require [clojure.string :as s]
            [clojure.test :refer [deftest is run-tests]]))

(def test-input "resources/year2024/day03/test-input")
(def puzzle-input "resources/year2024/day03/input")

;; Input parsing

(defn parse-input
  [input])

;; Part 1

(defn part01
  [input]
  (let [lines (-> (slurp input) s/split-lines)]
    (->> (for [line lines] (re-seq #"mul\((\d\d?\d?),(\d\d?\d?)\)" line))
         (apply concat)
         (map rest)
         (map #(map parse-long %))
         (map #(apply * %))
         (reduce + 0))))

(deftest test-part01
  (is (= 161 (part01 test-input)))
  (is (= 188192787 (part01 puzzle-input))))

;; Part 2

(defn part02
  [input])

(deftest test-part02)

(run-tests)
