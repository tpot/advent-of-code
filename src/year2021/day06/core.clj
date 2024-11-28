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

(defn timer-seq->timer-count
  "Convert seq of timer values to a vector of timer value counts"
  [fish-timers]
  (reduce
  (fn [fish-timers [timer-val count]]
    (update fish-timers timer-val + count))
  (vec (take 9 (repeat 0)))
  (-> (group-by identity fish-timers)
      (update-vals count))))

(defn part02
  [input generations]
  (->> (reduce
        (fn [tc _]
          (let [num-new-fish (get tc 0)]
            (-> (drop 1 tc) vec (conj 0)   ; Pop off mothers with zero timer
                (update 6 + num-new-fish)  ; Mother timers set to 6
                (update 8 + num-new-fish)  ; Baby timers set to 8
                )))
        (timer-seq->timer-count (parse-input input))
        (range generations))
       (apply +)))

(deftest test-part02
  (is (= 5934 (part02 test-input 80)))
  (is (= 387413 (part02 puzzle-input 80)))
  (is (= 1738377086345 (part02 puzzle-input 256))))

(run-tests)
