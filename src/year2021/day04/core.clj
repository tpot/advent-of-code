(ns year2021.day04.core
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [clojure.pprint :refer [pprint]]
            [clojure.test :refer [deftest is run-tests]]))

;; Input parsing

(def test-input "resources/year2021/day04/test-input")
(def puzzle-input "resources/year2021/day04/input")

(defn lines->board
  "Convert a seq of newline-separated strings of space-separated integers
   into a 2-dimensional seq of integers."
  [lines]
  (->> lines
       (map s/trim)
       (map #(s/split % #"\s+"))
       (map #(map parse-long %))
       (map vec)))

(defn parse-input
  [input]
  (let [lines (-> (slurp input) s/split-lines)
        draw-line (get lines 0)
        board-lines (->> (partition-by count (drop 2 lines))
                         (remove #(= (count %) 1)))]
    {:draw (vec (map parse-long (s/split draw-line #",")))
     :boards (map lines->board board-lines)}))

;; Helpers

(defn board-wins?
  "Return the board if any subset of numbers in the draw are equal to a
   row or column of the board, nil otherwise."
  [board draw]
  (let [row-groups (map set board)
        col-groups (map set (apply map vector board))]
    (when
     (some #(set/subset? % (set draw)) (concat row-groups col-groups))
      board)))

(deftest test-board-wins?
  (let [board [[1 2] [3 4]]]
    (is (some? (board-wins? board [1 2])))
    (is (some? (board-wins? board [1 4 2])))
    (is (nil? (board-wins? board [1 4])))
    (is (nil? (board-wins? board [2 3])))))

(defn draw-seq
  "A sequence of draws representing the bingo moves."
  [draw]
  (map #(take % draw) (range 1 (inc (count draw)))))

(deftest test-draw-seq
  (let [draw [1 2 3]]
    (is (= [[1] [1 2] [1 2 3]] (draw-seq draw)))))

(defn marked-numbers
  "Return the marked numbers from draw on board"
  [board draw]
  (set/intersection (set draw) (set (mapcat vec board))))

(deftest test-marked-numbers
  (is (= (set [1 2 3]) (marked-numbers [[1 2 3] [4 5 6]] [1 2 0 3]))))

(defn unmarked-numbers
  "Return the unmarked numbers from draw on board"
  [board draw]
  (let [all (set (mapcat vec board))
        marked (marked-numbers board draw)]
    (set/difference all marked)))

(deftest test-unmarked-numbers
  (is (= (set [4 5 6]) (unmarked-numbers [[1 2 3] [4 5 6]] [1 2 0 3]))))

(defn score-winner
  [winner]
  (->> (unmarked-numbers (get winner :board) (get winner :draw))
       (apply +)
       (* (last (get winner :draw)))))

;; Part 1

(defn part01
  "Find and score the first winning game"
  [input]
  (let [game (parse-input input)
        winner
        (reduce
         (fn [_ draw]
           (when-let [winner (some #(board-wins? % draw) (get game :boards))]
             (reduced {:board winner :draw draw})))   ; Break on first winner found
         nil
         (draw-seq (get game :draw)))]
    (score-winner winner)))

(deftest test-part01
  (is (= 4512 (part01 test-input)))
  (is (= 27027 (part01 puzzle-input))))

;; Part 2

(defn part02
  "Find and score the last winning game"
  [input]
  (let [game (parse-input input)
        state
        (reduce
         (fn [{:keys [won-boards open-boards] :as state} draw]
           (let [now-closed  (filter #(board-wins? % draw) open-boards)
                 still-open (remove #(board-wins? % draw) open-boards)]
             (if (seq now-closed)
               {:won-boards (concat won-boards (map #(zipmap [:board :draw] [% draw]) now-closed))
                :open-boards still-open}
               state)))
         {:winners []
          :open-boards (get game :boards)}
         (draw-seq (get game :draw)))
        last-winner (last (get state :won-boards))]
    (score-winner last-winner)))

(deftest test-part02
  (is (= 1924 (part02 test-input)))
  (is (= 36975 (part02 puzzle-input))))

(run-tests)
