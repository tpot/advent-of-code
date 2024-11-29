(ns year2021.day09.core
  (:require [clojure.string :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.test :refer [deftest is run-tests]]))

;; Input parsing

(def test-input "resources/year2021/day09/test-input")
(def puzzle-input "resources/year2021/day09/input")

(defn parse-line
  [line]
  (->> (s/split line #"")
       (map parse-long)
       vec))

(defn parse-input
  [input]
  (->> (-> (slurp input) s/split-lines)
       (map parse-line)
       vec))

;; Part 1

(defn adjacencies-seq
  "Return seq of all heights on the heightmap and that points adjacent heights"
  [{:keys [board w h]}]
  (map
   (fn [[row col val]]
     (->> [val
           (get-in board [(inc row) col]) (get-in board [(dec row) col])
           (get-in board [row (inc col)]) (get-in board [row (dec col)])]
          (remove nil?)))
   (for [row (range h) col (range w)]
     [row col (get-in board [row col])])))

(defn part01
  [input]
  (let [board (parse-input input)
        heightmap (zipmap [:board :w :h] [board (count (first board)) (count board)])]
    (->> (adjacencies-seq heightmap)
         (filter #(< (first %) (apply min (rest %))))
         (map first)
         (map inc)
         (apply +))))

(deftest test-part01
  (is (= 15 (part01 test-input)))
  (is (= 545 (part01 puzzle-input))))

;; Part 2

(defn part02
  [input])

(deftest test-part02)

(run-tests)
