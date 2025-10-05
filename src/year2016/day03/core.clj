(ns year2016.day03.core
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.test :refer [deftest is run-tests]]))

(def puzzle-input "resources/year2016/day03/input")

(defn parse-input
  [input]
  (->> (line-seq (clojure.java.io/reader input))
       (map s/trim)
       (map #(s/split % #"\s+"))
       (map #(map parse-long %))))

(defn triangle?
  [sides]
  (let [s (vec (sort sides))]
    (> (+ (get s 0) (get s 1)) (get s 2))))

(defn part01
  [input]
  (->> input (filter triangle?) count))

(deftest test-part01
  (is (= 1050 (part01 (parse-input puzzle-input)))))

(defn part02
  [input]
  (->> input
       (partition 3)
       (map #(apply mapv vector %))
       (apply concat)
       (filter triangle?)
       count))

(deftest test-part02
  (is (= 1921 (part02 (parse-input puzzle-input)))))

(run-tests)
