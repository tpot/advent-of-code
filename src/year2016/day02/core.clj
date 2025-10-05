(ns year2016.day02.core
  (:require [clojure.string :as s]
            [clojure.test :refer [deftest is run-tests]]))

(def test-input "resources/year2016/day02/test-input")
(def puzzle-input "resources/year2016/day02/input")

(defn parse-input
  [input]
  (->> (line-seq (clojure.java.io/reader input))
       (map #(map str %))
       (map #(map keyword %))))

(defn next-digit
  "Return next keypad digit after performing a move."
  [digit move]
  (let [m (mod (dec digit) 3)
        q (quot (dec digit) 3)]
    (condp = move
      :U (if-not (= 0 q) (- digit 3) digit)
      :D (if-not (= 2 q) (+ digit 3) digit)
      :L (if-not (= 0 m) (- digit 1) digit)
      :R (if-not (= 2 m) (+ digit 1) digit))))

(defn apply-moves
  "Return final keypad digit after applying a series of moves."
  [start-digit moves]
  (reduce
   (fn [digit move] (next-digit digit move))
   start-digit
   moves))

(deftest test-apply-moves
  (is (= 1 (apply-moves 5 [:U :L :L])))
  (is (= 9 (apply-moves 1 [:R :R :D :D :D])))
  (is (= 8 (apply-moves 9 [:L :U :R :D :L])))
  (is (= 5 (apply-moves 5 [:U :U :U :U :D]))))

(defn get-code
  "Given a starting digit and seq of moves, return bathroom code."
  [start-digit input]
  (loop [digit start-digit
         moves input
         result []]
    (let [new-digit (apply-moves digit (first moves))
          new-result (conj result new-digit)]
      (if-not (next moves)
        new-result
        (recur new-digit (next moves) new-result)))))

(defn part01
  [input]
  (get-code 5 input))

(deftest test-part01
  (is (= [1 9 8 5] (part01 (parse-input test-input))))
  (is (= [9 2 4 3 5] (part01 (parse-input puzzle-input)))))

(defn part02
  [input]
)

(deftest test-part02
)

(run-tests)
