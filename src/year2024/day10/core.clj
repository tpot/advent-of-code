(ns year2024.day10.core
  (:require [clojure.string :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.test :refer [deftest is run-tests]]))

(def test-file "resources/year2024/day10/test-input")
(def puzzle-file "resources/year2024/day10/input")

;; Input parsing

(defn find-element
  "Find instances of elements on a board"
  [board elt]
  (apply concat
         (for [row (range (count board))]
           (map
            #(vector row %)
            (keep-indexed (fn [i x] (when (= x elt) i)) (get board row))))))

(defn parse-input
  [file]
  (let [board (->> (-> (slurp file) s/split-lines)
                   (map #(map str %))
                   (mapv #(mapv parse-long %)))]
    {:board board
     :trail-heads (find-element board 0)}))

(defn north [[row col]] [(dec row) col])
(defn south [[row col]] [(inc row) col])
(defn east  [[row col]] [row (inc col)])
(defn west  [[row col]] [row (dec col)])

(defn trails
  "Return all trails through a map"
  [input]
  (let [{:keys [board trail-heads]} input]
    (loop [paths (map list trail-heads)
           trails []
           iter-count 10000]
      (when (zero? iter-count) (throw (Exception. "Iteration count exceeded")))
      (if (zero? (count paths))
        trails
        (let [path (first paths)
              cur-pos (first path)
              cur-height (get-in board cur-pos)
              dirs (map #(% cur-pos) [north south east west])
              new-dirs (->> dirs (filter #(= (get-in board %) (inc cur-height))))
              p (map #(cons % path) new-dirs)]
          (recur
           (if (= 8 cur-height) (rest paths) (concat (rest paths) p))
           (if (= 8 cur-height) (concat trails p) trails)
           (dec iter-count)))))))

;; Part 1

(defn part01
  [file]
  (let [found-trails  (trails (parse-input file))
        start-finish (map #(vector (last %) (first %)) found-trails)]
    (->> (update-vals (group-by first start-finish) #(count (set (map second %))))
         vals
         (apply +))))

(deftest test-part01
  (is (= 36 (part01 test-file)))
  (is (= 582 (part01 puzzle-file))))

;; Part 2

(defn part02
  [file]
  (let [found-trails (trails (parse-input file))
        trails-by-trailhead (map #(vector (last %) %) found-trails)]
    (->> (update-vals (group-by first trails-by-trailhead) #(count (set (map second %))))
         vals
         (apply +))))

(deftest test-part02
  (is (= 81 (part02 test-file)))
  (is (= 1302 (part02 puzzle-file))))

(run-tests)
