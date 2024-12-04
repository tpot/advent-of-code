(ns year2023.day09.core
  (:require [clojure.string :as s]
            [clojure.test :refer [deftest is]]))

(def test-input (slurp "resources/year2023/day09/test-input"))
(def puzzle-input (slurp "resources/year2023/day09/input"))

;; Input parsing

(defn parse-line
  [line]
  (map parse-long (-> line (s/split #" "))))

(defn parse-input
  [input]
  (->> (s/split-lines input)
       (map parse-line)))

(deftest test-parse-input
  (is (= [[0 3 6 9 12 15] [1 3 6 10 15 21] [10 13 16 21 30 45]]
         (parse-input test-input))))

;; Part 1

(defn seq-diff
  [xs]
  (reduce
   (fn [result input]
     (conj result (- (second input) (first input))))
   []
   (partition 2 1 xs)))

(deftest test-seq-diff
  (is (= [3 3 3 3 3] (seq-diff [0 3 6 9 12 15])))
  (is (= [0 0 0 0]   (seq-diff [3 3 3 3 3]))))

(defn sensor-history
  [xs]
  (let [xs-diffs (take-while
                  #(not (every? zero? %))
                  (iterate seq-diff xs))]
    (conj
     (vec xs-diffs)
     (-> (-> xs-diffs last count dec)
         (take (repeat 0))
         vec))))

(deftest test-sensor-history
  (is (= [[0 3 6 9 12 15] [3 3 3 3 3] [0 0 0 0]]
         (sensor-history [0 3 6 9 12 15]))))

(defn predicted-value
  [readings]
  (let [readings-history (->> (reverse (sensor-history readings))
                              (map reverse))]
    (ffirst
     (reduce
      (fn [result input]
        (conj result (conj input (+ (first input) (ffirst result)))))
      (list (first readings-history))
      (rest readings-history)))))

(deftest test-predicted-value
  (is (= [18 28 68] (map predicted-value (parse-input test-input)))))

(defn part01
  [input]
  (->> (parse-input input)
       (map predicted-value)
       (apply +)))

(deftest test-part01
  (is (= 114 (part01 test-input)))
  (is (= 1581679977 (part01 puzzle-input))))

;; Part 2

(defn predicted-value-first
  [readings]
  (let [readings-history (->> (reverse (sensor-history readings))
                              (map seq))] ; Argh why is (map seq) needed here?
    (ffirst
     (reduce
      (fn [result input]
        (conj result (conj input (- (first input) (ffirst result)))))
      (list (first readings-history))
      (rest readings-history)))))

(defn part02
  [input]
  (->> (parse-input input)
       (map predicted-value-first)
       (apply +)))

(deftest test-part02
  (is (= 2 (part02 test-input)))
  (is (= 889 (part02 puzzle-input))))
