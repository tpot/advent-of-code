(ns year2021.day03.core
  (:require [clojure.string :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.test :refer [deftest is run-tests]]))

;; Input parsing

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

;; Helpers

(defn count-zeros
  "Count zeros in a vector."
  [v]
  (->> v (filter zero?) count))

(defn count-ones
  "Cont ones in a vector."
  [v]
  (->> v (remove zero?) count))

(defn count-bits
  "Count the number of zero and one bits"
  [xs]
  (->> (apply map vector xs)
       (map #(vector (count-zeros %) (count-ones %)))
       vec))

(def powers-of-two (map #(bit-shift-left 1 %) (range)))

(defn bitarray->int
  "Convert array of bits to an integer"
  [xs]
  (->> xs reverse (map * powers-of-two) (apply +)))

;; Part 1

(defn part01
  [input]
  (let [report     (parse-input input)
        bit-counts (count-bits report)
        gamma      (map (fn [[zeros ones]] (if (> ones zeros) 1 0)) bit-counts)
        epsilon    (map (fn [[zeros ones]] (if (< ones zeros) 1 0)) bit-counts)]
    ; Convert gamma and epsilon from bit array to longs then multiply
    (->> [gamma epsilon]
         (map bitarray->int)
         (reduce * 1))))

(deftest test-part01
  (is (= 198 (part01 test-input)))
  (is (= 2640986 (part01 puzzle-input))))

;; Part 2

(defn reading
  "Calculate a reading with a bit criteria function"
  [input valfn]
  (let [report (parse-input input)
        reading (reduce
                 (fn [result ndx]
                   (let [bit-count (get (count-bits result) ndx)
                         newresult (filter #(when (= (get % ndx) (valfn bit-count)) true) result)]
                     (if (= 1 (count newresult))
                       (reduced (first newresult))
                       newresult)))
                 report
                 (range (count (first report))))]
    (bitarray->int reading)))

(defn oxy-reading
  "Get oxygen generator reading from when there are equal or more one bits"
  [input]
  (reading input #(if (>= (second %) (first %)) 1 0)))

(deftest test-oxy-reading
  (is (= 23 (oxy-reading test-input))))

(defn co2-reading
  "Get CO2 scrubber reading from when there are equal or more zero bits"
  [input]
  (reading input #(if (<= (first %) (second %)) 0 1)))

(deftest test-co2-reading
  (is (= 10 (co2-reading test-input))))

(defn part02
  [input]
  (* (oxy-reading input)(co2-reading input)))

(deftest test-part02
  (is (= 230 (part02 test-input)))
  (is (= 6822109 (part02 puzzle-input))))

(run-tests)
