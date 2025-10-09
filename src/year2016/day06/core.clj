(ns year2016.day06.core
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is run-tests]]))

(def test-input "resources/year2016/day06/test-input")
(def puzzle-input "resources/year2016/day06/input")

(defn parse-input
  [input]
  (->> (line-seq (clojure.java.io/reader input))
       (map #(apply vector %))))

(defn part01
  [input]
  (->> input
       (apply mapv vector)              ; Transpose
       (map frequencies)                ; Get letter frequences
       (map #(apply max-key val %))     ; Find map entry with largest value
       (map first)                      ; Get letter
       (apply str)))                    ; Convert to a string

(deftest test-part01
  (is (= "easter" (part01 (parse-input test-input))))
  (is (= "afwlyyyq" (part01 (parse-input puzzle-input)))))

(defn part02
  [input])

(deftest test-part02
)

(run-tests)
