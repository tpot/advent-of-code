(ns year2024.day01.core
  (:require [clojure.string :as s]
            [clojure.test :refer [deftest is run-tests]]))

(def test-input "resources/year2024/day01/test-input")
(def puzzle-input "resources/year2024/day01/input")

(defn parse-input
  [input]
  (->> (-> (slurp input) s/split-lines)
       (map #(s/split % #"   "))
       (map #(vector (parse-long (first %)) (parse-long (second %))))))

(defn part01
  [input]
  (let [input (parse-input input)
        left (sort (map first input))
        right (sort (map second input))]
    (reduce
     (fn [result [l r]]
       (+ result (abs (- l r))))
     0
     (map vector left right)))
  )

(deftest test-part01
  (is (= 11 (part01 test-input)))
  (is (= 1941353 (part01 puzzle-input))))

(defn part02
  [input]
  (let [lists (parse-input input)
        left (map first lists)
        right-count (update-vals
                     (->> lists
                          (map second)
                          (group-by identity))
                     count)]
    (reduce
     (fn [result input]
       (+ result (* input (or (get right-count input) 0))))
     0
     left)))

(deftest test-part02
  (is (= 31 (part02 test-input)))
  (is (= 22539317 (part02 puzzle-input))))

(run-tests)
