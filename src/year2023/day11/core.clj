(ns year2023.day11.core
  (:require [clojure.string :as s]
            [clojure.test :refer [deftest is]]))

(def test-input (slurp "resources/year2023/day11/test-input"))
(def test-input-expanded (slurp "resources/year2023/day11/test-input-expanded"))

(def puzzle-input (slurp "resources/year2023/day11/input"))

;; Parse input

(defn parse-input
  [input]
  (let [elts (s/split-lines input)]
    {:width (count (first elts))
     :height (count elts)
     :elts elts}))

(def GALAXY \#)
(def EMPTY  \.)

;; Helpers

(defn ndx->coordinate
  "Convert an index into elements into x, y coordinate."
  [grid ndx]
  [(mod ndx (:width grid)) (quot ndx (:width grid))])

(defn transpose
  "Transpose a grid"
  [grid]
  {:width  (:height grid)
   :height (:width grid)
   :elts   (map s/join (apply mapv vector (:elts grid)))})

(defn empty-row-indexes
  "Return indexes of empty rows in `grid`."
  [grid]
  (->> (:elts grid)
       (map-indexed #(vector %1 (every? (partial not= GALAXY) %2)))
       (filter #(true? (second %)))
       (map first)))

(defn insert-empty-row
  "Insert an empty row at index `ndx`"
  [grid ndx & char]
  {:width (:width grid)
   :height (inc (:height grid))
   :elts (let [[lhs rhs] (split-at ndx (:elts grid))
               empty-row (s/join (repeat (:width grid) (or (first char) EMPTY)))]
           (concat lhs [empty-row] rhs))})

(defn manhattan
  "Return Manhattan distance between two points."
  [a b]
  (+ (abs (- (first a)  (first b)))
     (abs (- (second a) (second b)))))

(deftest test-manhattan
  (is (= 9  (manhattan [5 11] [1 6])))
  (is (= 15 (manhattan [4 0]  [9 10])))
  (is (= 5  (manhattan [0 11] [5 11]))))

;; Part 1

(defn expand-universe
  [expansion-factor grid]
  (->> grid
       ;; Expand along y-axis
       (#(reduce
          (fn [grid row-ndx]
            (reduce
             (fn [grid _] (insert-empty-row grid row-ndx #_\X))
             grid
             (range expansion-factor)))
          % (map + (empty-row-indexes %) (map (partial * expansion-factor) (range)))))
       ;; Expand along x-axis
       transpose
       (#(reduce
          (fn [grid col-ndx]
            (reduce
             (fn [grid _] (insert-empty-row grid col-ndx #_\Y))
             grid
             (range expansion-factor)))
          % (map + (empty-row-indexes %) (map (partial * expansion-factor) (range)))))
       transpose))

(deftest test-expand-universe
  (is (= (parse-input test-input-expanded)
         (expand-universe 1 (parse-input test-input)))))

(defn combination [s]
  (let [tails (take-while next (iterate rest s))]
    (mapcat (fn [[f & rs]] (map #(vector f %) rs)) tails)))

(defn find-galaxies
  [universe]
  (->> (:elts universe)
       s/join
       (map-indexed vector)
       (filter #(= GALAXY (second %)))
       (map first)
       (map (partial ndx->coordinate universe))))

(deftest test-find-galaxies
  (let [grid (parse-input test-input)]
    (is (= 9 (count (find-galaxies grid))))))

(defn part01
  [input]
  (->> (parse-input input)
       (expand-universe 1)
       find-galaxies
       combination
       (map #(apply manhattan %))
       (reduce + 0)))

(deftest test-part01
  (is (= 374 (part01 test-input)))
  (is (= 9724940 (part01 puzzle-input))))

;; Part 2

;; StackOverFlow error when trying to create a matrix with an expansion factor
;; in the millions so use an alternate method.

(defn part02
  [expansion-factor input]
  (let [universe   (parse-input input)
        empty-rows (empty-row-indexes universe)
        empty-cols (empty-row-indexes (transpose universe))
        galaxies   (find-galaxies universe)]
    (->> galaxies
           ;; Increment y coordinates for empty rows
         (#(reduce
            (fn [result input]
              (let [[rs _] (split-at (inc input) empty-rows)]
                (let [row-ndx (last rs)
                      num-prev-rows (* expansion-factor (dec (count rs)))]
                  (map (fn [[x y]]
                         (if (> y (+ row-ndx num-prev-rows))
                           [x (+ y expansion-factor)]
                           [x y])) result))))
            (sort-by second %)
            (range (count empty-rows))))
           ;; Increment x coordinates for empty cols
         (#(reduce
            (fn [result input]
              (let [[cs _] (split-at (inc input) empty-cols)]
                (let [col-ndx (last cs)
                      num-prev-cols (* expansion-factor (dec (count cs)))]
                  (map (fn [[x y]]
                         (if (> x (+ col-ndx num-prev-cols))
                           [(+ x expansion-factor) y]
                           [x y])) result))))
            (sort-by first %)
            (range (count empty-cols))))
         combination
         (map #(apply manhattan %))
         (reduce + 0))))


;; TODO - figure out off-by-one error

(deftest test-part02
  (is (= 374     (part02 1 test-input)))
  (is (= 9724940 (part02 1 puzzle-input)))
  (is (= 1030    (part02 9 test-input)))
  (is (= 8410    (part02 99 test-input)))

  (is (= 569052586852 (part02 (dec 1000000) puzzle-input))))
  