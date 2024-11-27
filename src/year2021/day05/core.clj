(ns year2021.day05.core
  (:require [clojure.string :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.test :refer [deftest is run-tests]]))

;; Input parsing

(def test-input "resources/year2021/day05/test-input")
(def puzzle-input "resources/year2021/day05/input")

(defn parse-line
  [line]
  (let [[_ x1 y1 x2 y2] (re-matches #"(\d+),(\d+) -> (\d+),(\d+)" line)]
    [(map parse-long [x1 y1]) (map parse-long [x2 y2])]))

(defn parse-input
  [input]
  (->> (-> (slurp input) s/split-lines)
       (map parse-line)))

;; Helpers

(defn line-points
  "For straight lines return set of points between two ends"
  [[[x1 y1] [x2 y2]]]
  (when (or (= x1 x2) (= y1 y2))
    (for [x (range (min x1 x2) (inc (max x1 x2)))
          y (range (min y1 y2) (inc (max y1 y2)))]
      [x y])))

;; Part 1

(defn part01
  "Count number of intersections of two or more lines"
  [input]
  (->> (parse-input input)        ; Get list of points
       (map line-points)          ; Convert to sets of points
       (apply concat)             ; Convert to one big list
       (group-by identity)        ; Group equal points
       (map #(count (last %)))    ; Count elements of groups
       (filter #(>= % 2))         ; Count groups of more than two
       count))

(deftest test-part01
  (is (= 5 (part01 test-input)))
  (is (= 8350 (part01 puzzle-input))))

;; Part 2

(defn part02
  [input])

(deftest test-part02)

(run-tests)
