(ns year2021.day09.core
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [clojure.pprint :refer [pprint]]
            [clansi :as ansi]
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

;; Helpers

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn print-board
  [board & args]
  (doseq [row (range (count board))]
    (doseq [col (range (count (first board)))]
      (let [v (get-in board [row col])
            style (->> args
                       (map (fn [[style points]]
                              (when (contains? (set points) [row col]) style)))
                       (remove nil?)
                       last)]
        (print (if style (ansi/style (str v) style) v))))
    (print "\n")))

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

(defn south [row col] [(inc row) col])
(defn north [row col] [(dec row) col])
(defn east  [row col] [row       (inc col)])
(defn west  [row col] [row       (dec col)])

(defn adjacent-points
  "Return adjacent compass points for a given point"
  [board [row col]]
  (zipmap [:row :col :adj]
          [row col (zipmap [:n :s :e :w]
                           [(get-in board (north row col)) (get-in board (south row col))
                            (get-in board (east row col))  (get-in board (west row col))])]))

(defn find-basin
  "Find a basin around a low point by iterating over adjacent points that are
   continuous around a given point"
  [board [row col]]
      ; Visit adjacent points
  (loop [to-visit [[row col]]
         visited #{}
         max-iter 1000]
    (cond
      (zero? max-iter) (throw (Exception. "Iteration limit exceeded!"))
      (empty? to-visit) visited
      :else
      (let [[row col] (first to-visit)
            {:keys [n s e w]} (get (adjacent-points board [row col]) :adj)
            nearby-lows (->> [(when (and n (not= n 9)) (north row col))
                              (when (and s (not= s 9)) (south row col))
                              (when (and e (not= e 9)) (east row col))
                              (when (and w (not= w 9)) (west row col))]
                             (remove nil?)
                             set)]
        (recur
         (concat (set/difference nearby-lows visited) (rest to-visit))
         (conj visited [row col])
         (dec max-iter))))))

(defn part02
  [input]
  (let [board (parse-input input)
        w (count (first board))
        h (count board)
        lows (reduce
              (fn [result [row col]]
                (let [adj (-> board (adjacent-points [row col]) (get :adj) vals)]
                  (if (< (get-in board [row col])
                         (apply min (remove nil? adj)))
                    (conj result [row col])
                    result)))
              []
              (for [row (range h) col (range w)] [row col]))
        result (zipmap
                [:board :lows :basins]
                [board lows (map (partial find-basin board) lows)])]
    (->> (get result :lows)
         (map (partial find-basin (get result :board)))
         (map count)
         sort
         reverse
         (take 3)
         (reduce * 1))
    ))

(deftest test-part02
  (is (= 1134 (part02 test-input)))
  (is (= 950600 (part02 puzzle-input))))

(run-tests)
