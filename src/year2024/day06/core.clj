(ns year2024.day06.core
  (:require [clojure.string :as s]
            [clojure.set :as set]
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

(defn on-board?
  "Return true if a piece has moved off the board"
  [board pos]
  (not (nil? (get-in board pos))))

(defn run-guard
  [input]
  (loop [state (merge input {:visited #{}})
         max-iter 100000]
    #_(println "=>" (select-keys state [:guard-pos :guard-dir]))
    (cond
      (zero? max-iter)
      (throw (Exception. "Iteration limit exceeded!"))
      :else
      (let [{:keys [guard-pos guard-dir visited obstacles]} state
            new-guard-pos (condp = guard-dir
                            :n (north guard-pos)
                            :s (south guard-pos)
                            :e (east guard-pos)
                            :w (west guard-pos))
            stuck (get obstacles new-guard-pos)
            new-guard-dir (get {:n :e :e :s :s :w :w :n} guard-dir)
            new-state (merge state
                             {:visited (set/union visited #{guard-pos})}
                             (if stuck
                               {:guard-dir new-guard-dir}
                               {:guard-pos new-guard-pos}))]
        (if-not (on-board? (get state :board) new-guard-pos)
          new-state
          (recur new-state (dec max-iter)))))))

(defn part01
  [input]
  (-> (run-guard (parse-input input)) :visited count))

(deftest test-part01
  (is (= 41 (part01 test-input)))
  (is (= 4883 (part01 puzzle-input))))

;; Part 2

(defn part02
  [input])

(deftest test-part02)

(run-tests)
