(ns year2021.day01.core
  (:require [clojure.string :as s]
            [clojure.test :refer [deftest is run-tests]]))

(def test-input "resources/year2021/day01/test-input")
(def puzzle-input "resources/year2021/day01/input")

(defn parse-input
  [input]
  (map parse-long (-> (slurp input) s/split-lines)))

(defn part01
  [input]
  (->> (parse-input input)
       (partition 2 1)
       (filter #(> (second %) (first %)))
       count))

(deftest test-part01
  (is (= 7 (part01 test-input)))
  (is (= 1233 (part01 puzzle-input))))

(defn part02
  [input]
  (->> (parse-input input)
       (partition 3 1)
       (map #(apply + %))
       (partition 2 1)
       (filter #(> (second %) (first %)))
       count))

(deftest test-part02
  (is (= 5 (part02 test-input)))
  (is (= 1275 (part02 puzzle-input))))

(run-tests)
