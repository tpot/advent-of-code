(ns year2021.day06.core
  (:require [clojure.string :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.test :refer [deftest is run-tests]]))

;; Input parsing

(def test-input "resources/year2021/day06/test-input")
(def puzzle-input "resources/year2021/day06/input")

(defn parse-input
  [input]
  (map parse-long (-> (slurp input) s/trim (s/split #","))))

;; Part 1

(defn part01
  [input]
  (->> (reduce
        (fn [timers _]
          (let [num-new-fish (count (filter zero? timers))]
            (concat
             (map dec (remove zero? timers))
             (take num-new-fish (repeat 8))
             (take num-new-fish (repeat 6)))))
        (parse-input input)
        (range 80))
       count))

(deftest test-part01
  (is (= 5934 (part01 test-input)))
  (is (= 387413 (part01 puzzle-input))))

;; Part 2

(defn part02
  [input])

(deftest test-part02)

(run-tests)
