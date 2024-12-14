(ns year2024.day14.core
  (:require [clojure.string :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.test :refer [deftest is run-tests]]))

(def test-file "resources/year2024/day14/test-input")
(def puzzle-file "resources/year2024/day14/input")

(defn parse-line
  [line]
  (let [[px py vx vy]
        (->> line
             (re-matches #"p=(\d+),(\d+) v=(-?\d+),(-?\d+)")
             (drop 1)
             (map parse-long))]
    {:position [px py] :velocity [vx vy]}))

(defn parse-input
  [file]
  (mapv parse-line (-> (slurp file) s/split-lines)))

(defn quadrant
  "Determine which quadrant a point is part of."
  [x-max y-max [x y]]
  (let [x-mid (/ (dec x-max) 2)
        y-mid (/ (dec y-max) 2)]
    (cond
      (and (< x x-mid) (< y y-mid))
      :top-left
      (and (< x x-mid) (> y y-mid))
      :bottom-left
      (and (> x x-mid) (< y y-mid))
      :top-right
      (and (> x x-mid) (> y y-mid))
      :bottom-right)))

(defn calc-positions
  "Calculate final position for robots given board params."
  [robots & {:keys [x-max y-max] :as opts}]
  (let [num-rounds (or (:num-rounds opts) 100)]
    (->> robots
         (map #(vector
                (mod (+ (first (:position %))  (* num-rounds (first (:velocity %))))  x-max)
                (mod (+ (second (:position %)) (* num-rounds (second (:velocity %)))) y-max))))))

;; Part 1

(defn part01
  "Count the number of unique points the guard passed through."
  [file & {:keys [x-max y-max num-rounds]}]
  (let [robots (parse-input file)]
    (->> (calc-positions robots :x-max x-max :y-max y-max :num-rounds num-rounds)
         (map #(vector % (quadrant x-max y-max %)))
         (group-by second)
         (remove #(nil? (first %)))
         vals
         (map count)
         (apply *))))

(deftest test-part01
  (is (= 12        (part01 test-file   :x-max 11  :y-max 7)))
  (is (= 232589280 (part01 puzzle-file :x-max 101 :y-max 103))))

;; Part 2

(defn part02
  [file])

(deftest test-part02)

(run-tests)
