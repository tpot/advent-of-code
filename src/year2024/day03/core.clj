(ns year2024.day03.core
  (:require [clojure.string :as s]
            [clojure.test :refer [deftest is run-tests]]))

(def test-input1 "resources/year2024/day03/test-input1")
(def test-input2 "resources/year2024/day03/test-input2")
(def puzzle-input "resources/year2024/day03/input")

(defn parse-input
  [input]
  (-> (slurp input) s/split-lines s/join))

;; Part 1

(defn part01
  [input]
  (->> (re-seq #"mul\((\d\d?\d?),(\d\d?\d?)\)" (parse-input input))
       (map rest)
       (map #(map parse-long %))
       (map #(apply * %))
       (apply +)))

(deftest test-part01
  (is (= 161 (part01 test-input1)))
  (is (= 188192787 (part01 puzzle-input))))

;; Part 2

(defn part02
  [input]
  (->> (re-seq #"mul\((\d\d?\d?),(\d\d?\d?)\)|do\(\)|don\'t\(\)" (parse-input input))
       (map #(cond
               ; do() instruction
               (= (first %) "do()")
               {:op :do}
               ; don't() instruction
               (= (first %) "don't()")
               {:op :dont}
               ; mul() instruction
               (s/starts-with? (first %) "mul")
               (zipmap [:op :arg1 :arg2] (concat [:mul] (map parse-long (drop 1 %))))))
       (reduce
        (fn [{:keys [state count] :as result} {:keys [op arg1 arg2]}]
          (condp = op
            :do   (into result {:state :do})
            :dont (into result {:state :dont})
            :mul  (into result {:count (+ count (if (= state :do) (* arg1 arg2) 0))})))
        {:state :do :count 0})
       :count))

(deftest test-part02
  (is (= 48 (part02 test-input2)))
  (is (= 113965544 (part02 puzzle-input))))

(run-tests)
