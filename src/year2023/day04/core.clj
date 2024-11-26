(ns year2023.day04.core
   (:require [clojure.string :as s]
             [clojure.test :refer [deftest is]]
             [clojure.math :refer [pow]]
             [clojure.set :as set]))

 (def puzzle-input (slurp "resources/year2023/day04/input"))
 (def test-input (slurp "resources/year2023/day04/test-input"))

;; Input parsing

 (defn parse-line
   "Parse a line of text into a map of card number, winning and the actual numbers
   we scratched off."
   [line]
   (let [[_ card-num winning actual]
         (re-find #"Card\s+(\d+):\s+(.*)\s+\|\s+(.*)" line)]
     {:card-num (parse-long card-num)
      :winning  (set (map parse-long (s/split winning #"\s+")))
      :actual   (set (map parse-long (s/split actual #"\s+")))}))

;; Part 1

;; Calculate the common elements on each card, score the card, then add up the
;; scores for all cards.

 (defn score
   [num-cards]
   (if (> num-cards 0) (int (pow 2 (dec num-cards))) 0))

 (defn part01
   [input]
   (->> (map parse-line (s/split-lines input))
        (map #(set/intersection (:winning %) (:actual %)))
        (map #(score (count %)))
        (reduce + 0)))

 (deftest test-part01
   (is (= 13 (part01 test-input)))
   (is (= 18653 (part01 puzzle-input))))

;; Part 2

;; More tricky but the solution is making a single iteration over a vector of
;; all ones representing our initial cards and updating the number of copies of
;; tickets in our hand with the winnings for the current card.

(defn num-matched
  "Return number of extra tickets won for `card`"
  [card]
  (->> card (#(set/intersection (:winning %) (:actual %))) count))

(defn enriched-cards
  "Enrich cards by adding a key for the cards won by this card."
  [input]
  (loop [result []
         cards (map parse-line (s/split-lines input))]
    (let [card (first cards)
          matches (num-matched card)
          wins-cards (map :card-num (take matches (rest cards)))
          new-result (conj result
                           (assoc card :wins-cards wins-cards))]
      (if-not (next cards)
        new-result
        (recur new-result (rest cards))))))

(defn part02
  [input]
  (let [cards (enriched-cards input)]
    (loop [total-cards 0
           counts (vec (take (count cards) (repeat 1)))]
      (let [ndx (first (keep-indexed #(when (> %2 0) %1) counts))
            count (get counts ndx)
            won-cards (:wins-cards (get cards ndx))
            new-counts (assoc (reduce
                               (fn [result input] (update result (dec input) + count))
                               counts won-cards)
                              ndx 0)]
        (if (zero? (apply + new-counts))
          (+ total-cards count)
          (recur (+ total-cards count) new-counts))))))

(deftest test-part02
  (is (= 30 (part02 test-input)))
  (is (= 5921508 (part02 puzzle-input))))
