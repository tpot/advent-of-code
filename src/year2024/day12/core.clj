(ns year2024.day12.core
  (:require [clojure.string :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.set :as set]
            [clojure.test :refer [deftest is run-tests]]))

(def test-file "resources/year2024/day12/test-input")
(def puzzle-file "resources/year2024/day12/input")

(defn parse-input
  [input]
  (->> (-> (slurp input) s/split-lines)
       (mapv #(s/split % #""))))

(defn north [[row col]] [(dec row) col])
(defn south [[row col]] [(inc row) col])
(defn east  [[row col]] [row (inc col)])
(defn west  [[row col]] [row (dec col)])

(defn c-points
  "Return compass points from a given point."
  [[row col]]
  (map #(% [row col]) [north south east west]))

(defn find-region
  "Given a point explore find the region this point is a part of."
  [board point]
  (let [crop-type (get-in board point)]
    (loop [points-to-check [point]
           region #{point}
           iter-count 1050]
      (when (zero? iter-count) (throw (Exception. "Iteration count exeeded in find-region")))
      (if (empty? points-to-check)
        {:crop-type crop-type :region region}
        (let [p (first points-to-check)
              found-crops (->> p c-points (filter #(= crop-type (get-in board %))) set)]
          (recur
           (concat (rest points-to-check) (into [] (set/difference found-crops region (set points-to-check))))
           (set/union region found-crops)
           (dec iter-count)))))))

(defn find-regions
  "Find all crop regions on a board"
  [board]
  (let [all-points (set
                    (for [row (range (count board))
                          col (range (count (first board)))]
                      [row col]))]
    (loop [points all-points
           regions []
           iter-count 1000]
      (when (zero? iter-count) (throw (Exception. "Iteration count exeeded in top-level loop")))
      (if (empty? points)
        {:board board :regions regions}
        (let [point (first points)
              {:keys [region] :as crop-region} (find-region board point)]
          (recur
           (set/difference points region)
           (conj regions crop-region)
           (dec iter-count)))))))

(defn perimeter
  "Return perimeter of a region."
  [region]
  (reduce
   (fn [result p]
     (+ (->> (c-points p)
             (map #(if (contains? region %) 0 1))
             (apply +))
        result))
   0
   region))

(defn area
  "Return area of a region."
  [region]
  (count region))

;; Part 1

(defn part01
  [file]
  (let [regions (->> (parse-input file) find-regions :regions (map :region))]
    (->> (map vector (map area regions) (map perimeter regions))
         (map (partial apply *))
         (apply +))))

(deftest test-part01
  (is (= 1930 (part01 test-file)))
  (is (= 1319878 (part01 puzzle-file))))

;; Part 2

(defn part02
  [input])

(deftest test-part02)

(run-tests)
