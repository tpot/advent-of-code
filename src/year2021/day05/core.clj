(ns year2021.day05.core
  (:require [clojure.string :as s]
            [clojure.math :as math]
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

(defn straight-line-points
  "For straight lines return set of points between two ends"
  [[[x1 y1] [x2 y2]]]
  (when (or (= x1 x2) (= y1 y2))
    (for [x (range (min x1 x2) (inc (max x1 x2)))
          y (range (min y1 y2) (inc (max y1 y2)))]
      [x y])))

(defn signum
  "Return sign of an integer"
  [x]
  (int (math/signum x)))

(defn diagonal-line-points
  "For diagonal lines set return of points between two ends"
  [[[x1 y1] [x2 y2]]]
  (let [x-delta (- x2 x1)
        y-delta (- y2 y1)]
    (when (= (abs x-delta) (abs y-delta))
      (map vector
           (range x1 (+ x2 (signum x-delta)) (signum x-delta))
           (range y1 (+ y2 (signum y-delta)) (signum y-delta))))))

;; Part 1

(defn part01
  "Count number of intersections of two or more lines"
  [input]
  (->> (parse-input input)
       (map straight-line-points) ; Get list of points
       (apply concat)             ; Merge into single list
       (group-by identity)        ; Make groups of points
       (map #(count (last %)))    ; Count elements of groups
       (filter #(>= % 2))         ; Count groups of more than two
       count))

(deftest test-part01
  (is (= 5 (part01 test-input)))
  (is (= 8350 (part01 puzzle-input))))

;; Part 2

(defn part02
  [input]
  (let [lines (parse-input input)]
    (->> (concat (map straight-line-points lines)  ; Get list of points
                 (map diagonal-line-points lines))
         (apply concat)                            ; Merge into single list
         (group-by identity)                       ; Convert to one big list
         (map #(count (last %)))                   ; Count elements of groups
         (filter #(>= % 2))                        ; Count groups of more than two
         count)))

(deftest test-part02
  (is (= 12 (part02 test-input)))
  (is (= 19374 (part02 puzzle-input))))

(run-tests)
