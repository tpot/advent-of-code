(ns year2021.day03.core
  (:require [clojure.string :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.test :refer [deftest is run-tests]]))

(def test-input "resources/year2021/day03/test-input")
(def puzzle-input "resources/year2021/day03/input")

(defn parse-line
  [line]
  (->> line
       seq
       (mapv int)
       (mapv #(- % 48))))

(defn parse-input
  [input]
  (map parse-line (-> (slurp input) s/split-lines)))

(defn count-zeros
  "Count zeros in a vector."
  [v]
  (->> v (filter zero?) count))

(defn count-ones
  "Cont ones in a vector."
  [v]
  (->> v (remove zero?) count))

(defn gamma_bit
  "Gamma bit set if more ones than zeros."
  [[zeros ones]]
  (if (> ones zeros) 1 0))

(defn epsilon_bit
  "Epsilon bit set if more zeros than ones."
  [[zeros ones]]
  (if (< ones zeros) 1 0))

(def powers-of-two (map #(bit-shift-left 1 %) (range)))

(defn part01
  [input]
  (let [report     (parse-input input)
        bit_counts (->> (apply map vector report)
                        (map #(vector (count-zeros %) (count-ones %))))
        gamma      (map gamma_bit bit_counts)
        epsilon    (map epsilon_bit bit_counts)]
    (reduce * 1 (map #(->> % reverse (map * powers-of-two) (reduce + 0)) [gamma epsilon]))))

(deftest test-part01
  (is (= 198 (part01 test-input)))
  (is (= 2640986 (part01 puzzle-input))))

(defn part02
  [input])

(deftest test-part02)

(run-tests)
