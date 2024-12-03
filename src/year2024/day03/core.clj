(ns year2024.day03.core
  (:require [clojure.string :as s]
            [clojure.test :refer [deftest is run-tests]]))

(def test-input1 "resources/year2024/day03/test-input1")
(def test-input2 "resources/year2024/day03/test-input2")
(def puzzle-input "resources/year2024/day03/input")

;; Input parsing

(defn parse-input
  [input]
  (-> (slurp input) s/split-lines))

;; Part 1

(defn part01
  [input]
  (let [lines (parse-input input)]
    (->> (for [line lines] (re-seq #"mul\((\d\d?\d?),(\d\d?\d?)\)" line))
         (apply concat)
         (map rest)
         (map #(map parse-long %))
         (map #(apply * %))
         (reduce + 0))))

(deftest test-part01
  (is (= 161 (part01 test-input1)))
  (is (= 188192787 (part01 puzzle-input))))

;; Part 2

(defn part02
  [input]
  (let [lines (parse-input input)]
    (->> (for [line lines] (re-seq #"mul\((\d\d?\d?),(\d\d?\d?)\)|do\(\)|don\'t\(\)" line))
         (apply concat)
         (map #(cond
                 ; mul() instruction
                 (s/starts-with? (first %) "mul")
                 (zipmap [:op :arg1 :arg2] (concat [:mul] (map parse-long (drop 1 %))))
                 ; don't() instruction
                 (= (first %) "don't()")
                 {:op :dont}
                 ; do() instruction
                 (= (first %) "do()")
                 {:op :do}))
         (reduce
          (fn [{:keys [state count] :as result} {:keys [op arg1 arg2]}]
            (condp = op
              :dont (into result {:state :dont})
              :do   (into result {:state :do})
              :mul  (into result {:count (+ count (if (= state :do) (* arg1 arg2) 0))})))
          {:state :do :count 0})
         :count)))

(deftest test-part02
  (is (= 48 (part02 test-input2)))
  (is (= 113965544 (part02 puzzle-input))))

(run-tests)
