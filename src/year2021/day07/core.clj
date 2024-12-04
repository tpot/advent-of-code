(ns year2021.day07.core
  (:require [clojure.string :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.test :refer [deftest is run-tests]]))

;; Input parsing

(def test-input "resources/year2021/day07/test-input")
(def puzzle-input "resources/year2021/day07/input")

(defn parse-input
  [input]
  (map parse-long (-> (slurp input) s/trim (s/split #","))))

;; Helpers

(defn min-index
  [xs]
  (let [min-val (apply min xs)]
    (->> xs
         (keep-indexed #(when (= %2 min-val) %1))
         first)))

(defn find-cheapest-row
  [input cost-fn]
  (let [crabs (parse-input input)
        max-row (apply max crabs)
        costs (reduce
               (fn [result row]
                                ; Calculate fuel used to move to row
                 (let [cost (->> crabs
                                 (map #(cost-fn % row))
                                 (apply +))]
                   (conj result cost)))
               []
               (range max-row))
        row (min-index costs)]
    (get costs row)))

;; Part 1

(defn calc-cost-part1
  "Calculate the cost of moving a crab from one row to another"
  [from to]
  (abs (- from to)))

(defn part01
  [input]
  (find-cheapest-row input calc-cost-part1))

(deftest test-part01
  (is (= 37 (part01 test-input)))
  (is (= 328262 (part01 puzzle-input))))

;; Part 2

(defn calc-cost-part2
  "Calculate cost where fuel usage increases the further you move"
  [from to]
  (->> (range)
       (drop 1)
       (take (abs (- from to)))
       (apply +)))

(defn part02
  [input]
  (find-cheapest-row input calc-cost-part2))

(deftest test-part02
  (is (= 168 (part02 test-input)))
  (is (= 90040997 (part02 puzzle-input))))

(run-tests)
