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

;; Part 1

(defn min-index
  [xs]
  (let [min-val (apply min xs)]
    (->> xs
         (keep-indexed #(when (= %2 min-val) %1))
         first)))

(defn part01
  [input]
  (let [crabs (parse-input input)
        max-row (apply max crabs)]
    (let [costs (reduce
                 (fn [result row]
                   ; Calculate fuel used to move to row
                   (let [cost (->> crabs
                                   (map #(abs (- % row)))
                                   (apply +))]
                     (conj result cost)))
                 []
                 (range max-row))
          row (min-index costs)]
      (get costs row))))

(deftest test-part01
  (is (= 37 (part01 test-input)))
  (is (= 328262 (part01 puzzle-input))))

;; Part 2

(defn part02
  [input])

(deftest test-part02)

(run-tests)
