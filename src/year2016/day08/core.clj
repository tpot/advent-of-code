(ns year2016.day08.core
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :as set]
            [clojure.test :refer [deftest is run-tests]]))

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

(def MAX-X 50)
(def MAX-Y 6)

(defn rect-points
  "All points in a rectangular area."
  [x1 y1 x2 y2]
  (set
   (for [x (range (min x1 x2) (max x1 x2))
         y (range (min y1 y2) (max y1 y2))]
     [x y])))

(defn col-points
  "All points in a column; x is constant"
  [x]
  (set (map vector (repeat x) (range 0 MAX-Y))))

(defn row-points
  "All points in a row; y is constant"
  [y]
  (set (map vector (range 0 MAX-X) (repeat y))))

(def empty-screen (vec (take MAX-Y (repeat (vec (take MAX-X (repeat \.)))))))

(defn screen->str
  "Convert screen of points to a string."
  [screen-points]
  (->> screen-points
       (reduce
        (fn [screen input]
          (update-in screen (reverse input) (constantly \#)))
        empty-screen)
       (map #(s/join %))
       (s/join "\n")))

(defn rotate-column
  [screen-points n by]
  (let [col (col-points n)
        on-in-col (set/intersection screen-points col)
        rot-in-col (set (map (fn [[x y]] (vector x (mod (+ by y) MAX-Y))) on-in-col))]
    (set/union
     (set/difference screen-points on-in-col)
     rot-in-col)))

(defn rotate-row
  [screen-points n by]
  (let [row (row-points n)
        on-in-row (set/intersection screen-points row)
        rot-in-row (set (map (fn [[x y]] (vector (mod (+ by x) MAX-X) y)) on-in-row))]
    (set/union
     (set/difference screen-points on-in-row)
     rot-in-row)))

(defn do-instructions
  [instrs & {:keys [debug] :or {debug false}}]
  (reduce
   (fn [screen-points instr]
     (condp = (get instr :cmd)
       :rect (let [{:keys [width height]} instr]
               (when debug (println "rect" width "x" height))
               (set/union screen-points (rect-points 0 0 width height)))
       :rotate-row (let [{:keys [count by]} instr]
                     (when debug (println "rotate-row" "count" count "by" by))
                     (rotate-row screen-points count by))
       :rotate-column (let [{:keys [count by]} instr]
                        (when debug (println "rotate-column" "count" count "by" by))
                        (rotate-column screen-points count by))))
   #{}
   instrs))

(defn part01
  [input]
  (count (do-instructions input)))

(deftest test-part01
  (is (= 116 (part01 (parse-input puzzle-input)))))

(defn part02
  [input]
  (do-instructions input))

;; Answer obtained by inspection:
;;
;; (-> (parse-input puzzle-input)
;;     part02
;;     screen->str
;;     println)
;; ; #..#.###...##....##.####.#....###...##..####.####.
;; ; #..#.#..#.#..#....#.#....#....#..#.#..#.#.......#.
;; ; #..#.#..#.#..#....#.###..#....###..#....###....#..
;; ; #..#.###..#..#....#.#....#....#..#.#....#.....#...
;; ; #..#.#....#..#.#..#.#....#....#..#.#..#.#....#....
;; ; .##..#.....##...##..#....####.###...##..####.####.

(run-tests)
