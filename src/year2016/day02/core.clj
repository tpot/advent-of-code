(ns year2016.day02.core
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is run-tests]]))

;; Puzzle input

(def test-input "resources/year2016/day02/test-input")
(def puzzle-input "resources/year2016/day02/input")

(defn parse-input
  [input]
  (->> (line-seq (clojure.java.io/reader input))
       (map #(map str %))
       (map #(map keyword %))))

;; Part 1

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

;; Part 2

(def KEYPAD-LAYOUT
  [[:0 :0 :1 :0 :0]
   [:0 :2 :3 :4 :0]
   [:5 :6 :7 :8 :9]
   [:0 :A :B :C :0]
   [:0 :0 :D :0 :0]])

(def KEYPAD-LOOKUP
  (apply
   merge
   (for [row (range 5) col (range 5)]
     (let [coord [row col] key (get-in KEYPAD-LAYOUT [row col])]
       (into {} [[coord key] [key coord]])))))

(defn next-key
  "Return next keypad digit after performing a move for new keypad design."
  [key move]
  (let [[x y] (get KEYPAD-LOOKUP key)
        [new-x new-y] (condp = move
                        :U (if-not (= 0 x) [(dec x) y] [x y])
                        :D (if-not (= 4 x) [(inc x) y] [x y])
                        :L (if-not (= 0 y) [x (dec y)] [x y])
                        :R (if-not (= 4 y) [x (inc y)] [x y]))
        new-key (get KEYPAD-LOOKUP [new-x new-y])]
    (if (not= :0 new-key) new-key key)))

(deftest test-next-key
  (is (= :5 (next-key :5 :U)))
  (is (= :5 (next-key :5 :D)))
  (is (= :5 (next-key :5 :L)))
  (is (= :5 (next-key :5 :L))))

(defn apply-moves-part02
  "Return final keypad digit after applying a series of moves."
  [start-key moves]
  (reduce
   (fn [key move] (next-key key move))
   start-key
   moves))

(deftest test-apply-moves-part02
  (is (= :5 (apply-moves-part02 :5 [:U :L :L])))
  (is (= :D (apply-moves-part02 :5 [:R :R :D :D :D])))
  (is (= :B (apply-moves-part02 :D [:L :U :R :D :L])))
  (is (= :3 (apply-moves-part02 :B [:U :U :U :U :D]))))

(defn get-code-part02
  "Given a starting digit and seq of moves, return bathroom code."
  [start-key input]
  (loop [key start-key
         moves input
         result []]
    (let [new-key (apply-moves-part02 key (first moves))
          new-result (conj result new-key)]
      (if-not (next moves)
        new-result
        (recur new-key (next moves) new-result)))))

(defn part02
  [input]
  (get-code-part02 :5 input))

(deftest test-part02
  (is (= [:5 :D :B :3] (part02 (parse-input test-input))))
  (is (= [:C :1 :A :8 :8] (part02 (parse-input puzzle-input)))))

(run-tests)
