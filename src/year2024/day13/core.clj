(ns year2024.day13.core
  (:require [clojure.string :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.math.combinatorics :as combo]
            [clojure.test :refer [deftest is run-tests]]))

(def test-file "resources/year2024/day13/test-input")
(def puzzle-file "resources/year2024/day13/input")

(defn parse-entry
  "Parse an entry of button config and prize location"
  [lines]
  (zipmap [:button-a-x :button-a-y :button-b-x :button-b-y :prize-x :prize-y]
          (->> (re-matches #"Button A: X\+(\d+), Y\+(\d+) Button B: X\+(\d+), Y\+(\d+) Prize: X=(\d+), Y=(\d+)" (s/join " " lines))
               (drop 1)
               (map parse-long))))

(defn parse-input
  [file]
  (->> (-> (slurp file) s/split-lines)
       (partition-by #(= "" %))
       (remove #(= 1 (count %)))
       (map parse-entry)))

;; Part 1

(defn sat?
  "Does a choice of presses for buttons A and B satisfy the constraints
   of the problem?"
  [entry [a b]]
  (= [(get entry :prize-x) (get entry :prize-y)]
     [(+ (* a (get entry :button-a-x))
         (* b (get entry :button-b-x)))
      (+ (* a (get entry :button-a-y))
         (* b (get entry :button-b-y)))]))

(defn solutions
  "Return solutions for the problem."
  [entry]
  (->> (combo/cartesian-product (range 100) (range 100))
       (filter #(sat? entry %))))

(defn tokens-used
  [[a b]]
  (+ (* a 3) b))

(defn part01
  "Count the number of unique points the guard passed through."
  [file]
  (let [entries (parse-input file)]
    (->> entries
         (map solutions)
         (remove empty?)
         (mapcat #(map tokens-used %))
         (apply +)))
  )

(deftest test-part01
  (is (= 480 (part01 test-file)))
  (is (= 27105 (part01 puzzle-file))))

;; Part 2

(defn part02
  "Get the path taken by an initial run through the maze and then use it
   to seed possible locations for an extra obstacle."
  [file])

(deftest test-part02)

(run-tests)
