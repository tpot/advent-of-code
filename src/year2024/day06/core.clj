(ns year2024.day06.core
  (:require [clojure.string :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.test :refer [deftest is run-tests]]))

(def test-input "resources/year2024/day06/test-input")
(def puzzle-input "resources/year2024/day06/input")

(defn find-pieces
  "Find instances of pieces on a board"
  [board ch]
  (apply concat
         (for [row (range (count board))]
           (map
            #(vector row %)
            (keep-indexed (fn [i x] (when (= x ch) i)) (get board row))))))

(defn parse-input
  "Return a board and list of guards"
  [input]
  (let [board (-> (slurp input) s/split-lines)]
    {:board     board
     :guard-pos (first (find-pieces board \^))
     :guard-dir :n
     :obstacles (set (find-pieces board \#))}))

(defn north      [[row col]] [(dec row) col])
(defn south      [[row col]] [(inc row) col])
(defn east       [[row col]] [row       (inc col)])
(defn west       [[row col]] [row       (dec col)])

;; Part 1

(defn right-turn
  "Return direction after a right turn"
  [dir]
  (get {:n :e :e :s :s :w :w :n} dir))

(defn move
  [pos dir]
  (condp = dir
    :n (north pos)
    :s (south pos)
    :e (east pos)
    :w (west pos)))

(defn on-board?
  "Return true if a piece has moved off the board"
  [board pos]
  (not (nil? (get-in board pos))))

(defn run-guard
  "Run the guard through their paces. The resulting state contains the
   final result whether the guard was in a loop or went out of bounds,
   as well as their path through the maze."
  [input]
  (loop [state (merge input {:path [] :track-set #{}})
         max-iter 10000]
    (when (zero? max-iter)
      (throw (ex-info "Iteration limit exceeded!" {:state state})))

    (let [{:keys [guard-pos guard-dir path track-set obstacles]} state
          new-guard-pos (move guard-pos guard-dir)
          new-guard-dir (right-turn guard-dir)
          blocked? (get obstacles new-guard-pos)
          new-state (merge state
                           {:path (conj path [guard-pos guard-dir])}
                           {:track-set (conj track-set [guard-pos guard-dir])}
                           (if blocked?
                             {:guard-dir new-guard-dir}
                             {:guard-pos new-guard-pos}))]
      (cond
        ; Guard has gone out of bounds
        (not (on-board? (get state :board) new-guard-pos))
        (merge new-state {:result :guard-out-of-bounds})

        ; Guard has started on a loop
        (contains? track-set [guard-pos guard-dir])
        (merge new-state {:result :guard-stuck-in-loop})

        ; Make next move
        :else
        (recur new-state (dec max-iter))))))

(defn part01
  "Count the number of unique points the guard passed through."
  [input]
  (->> (run-guard (parse-input input))
       :path
       (map first)
       set
       count))

(deftest test-part01
  (is (= 41 (part01 test-input)))
  (is (= 4883 (part01 puzzle-input))))

;; Part 2

(defn part02
  "Get the path taken by an initial run through the maze and then use it
   to seed possible locations for an extra obstacle."
  [file]
  (let [{:keys [obstacles guard-pos] :as input} (parse-input file)
        {:keys [path]} (run-guard input)]
    (->>
     (map
      #(let [new-input (assoc input :obstacles (conj obstacles %))]
         (run-guard new-input))
      (disj (set (map first path)) guard-pos)) ; Don't count initial guard pos
     (map :result)
     (filter #(= % :guard-stuck-in-loop))
     count)))

(deftest test-part02
  (is (= 6 (part02 test-input)))
  (is (= 1655 (part02 puzzle-input))))

(run-tests)
