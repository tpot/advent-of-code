(ns year2016.day08.core
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :as set]
            [clojure.test :refer [deftest is run-tests]]))

(def MAX-X 50)
(def MAX-Y 6)

(def puzzle-input "resources/year2016/day08/input")

(defn parse-line
  [s]
  (condp re-matches s
    ;; Draw rect
    #"^rect (\d+)x(\d+)"
    :>> (fn [[_ width height]]
          {:cmd :rect :width (parse-long width) :height (parse-long height)})
    ;; Rotate row or column
    #"rotate (row|column) (x|y)=(\d+) by (\d+)"
    :>> (fn [[_ row-or-col _ count by]]
          {:cmd (keyword (str "rotate-" row-or-col)) :count (parse-long count) :by (parse-long by)})))

(deftest test-parse-line
  (is (= {:cmd :rect,          :width 2, :height 4} (parse-line "rect 2x4")))
  (is (= {:cmd :rotate-row,    :count 1, :by     2} (parse-line "rotate row y=1 by 2")))
  (is (= {:cmd :rotate-column, :count 3, :by     4} (parse-line "rotate column x=3 by 4"))))

(defn parse-input
  [input]
  (->> (line-seq (clojure.java.io/reader input))
       (map parse-line)))

(defn grid-new
  "Return an empty grid of given dimensions."
  [width height]
  {:width  width
   :height height
   :points #{}})

(defn grid-set-points
  "Set points on a grid"
  [grid xs]
  (update grid :points #(set/union % xs)))

(defn grid-clear-points
  "Clear points on a grid"
  [grid xs]
  (update grid :points #(set/difference % xs)))

(defn grid->str
  "Convert grid to a string."
  [{:keys [width height points]}]
  (let [grid-str (take height (repeat (take width (repeat \.))))]
    (->> points
         (reduce
          (fn [result input]
            (update-in result (reverse input) (constantly \#)))
          (mapv #(mapv identity %) grid-str))
         (map #(s/join %))
         (s/join "\n"))))

(defn grid-points-in-region
  "Return points set within a region."
  [grid xs]
  (set/intersection (get grid :points) xs))

(defn rect-points
  "All points in a rectangular area."
  [x1 y1 x2 y2]
  (set
   (for [x (range (min x1 x2) (max x1 x2))
         y (range (min y1 y2) (max y1 y2))]
     [x y])))

(defn col-points
  "All points in a column; x is constant"
  [grid x]
  (set (map vector (repeat x) (range 0 (get grid :height)))))

(defn row-points
  "All points in a row; y is constant"
  [grid y]
  (set (map vector (range 0 (get grid :width)) (repeat y))))

(defn rotate-column
  [grid n by]
  (let [col (col-points grid n)
        on-in-col (grid-points-in-region grid col)
        rot-in-col (set (map (fn [[x y]] (vector x (mod (+ by y) (get grid :height)))) on-in-col))]
    (-> grid
        (grid-clear-points col)
        (grid-set-points rot-in-col))))

(defn rotate-row
  [grid n by]
  (let [row (row-points grid n)
        on-in-row (grid-points-in-region grid row)
        rot-in-row (set (map (fn [[x y]] (vector (mod (+ by x) (get grid :width)) y)) on-in-row))]
    (-> grid
        (grid-clear-points row)
        (grid-set-points rot-in-row))))

(defn do-instructions
  [instrs & {:keys [debug] :or {debug false}}]
  (reduce
   (fn [grid instr]
     (condp = (get instr :cmd)
       :rect          (let [{:keys [width height]} instr]
                        (when debug (println "rect" width "x" height))
                        (grid-set-points grid (rect-points 0 0 width height)))
       :rotate-row    (let [{:keys [count by]} instr]
                        (when debug (println "rotate-row" "count" count "by" by))
                        (rotate-row grid count by))
       :rotate-column (let [{:keys [count by]} instr]
                        (when debug (println "rotate-column" "count" count "by" by))
                        (rotate-column grid count by))))
   (grid-new MAX-X MAX-Y)
   instrs))

(defn part01
  [input]
  (-> input
      do-instructions
      :points
      count))

(deftest test-part01
  (is (= 116 (part01 (parse-input puzzle-input)))))

(defn part02
  [input]
  (do-instructions input))

;; Answer obtained by inspection:
;;
;; (-> (parse-input puzzle-input)
;;     part02
;;     grid->str
;;     println)
;; ; #..#.###...##....##.####.#....###...##..####.####.
;; ; #..#.#..#.#..#....#.#....#....#..#.#..#.#.......#.
;; ; #..#.#..#.#..#....#.###..#....###..#....###....#..
;; ; #..#.###..#..#....#.#....#....#..#.#....#.....#...
;; ; #..#.#....#..#.#..#.#....#....#..#.#..#.#....#....
;; ; .##..#.....##...##..#....####.###...##..####.####.

(run-tests)
