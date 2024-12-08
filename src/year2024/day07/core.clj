(ns year2024.day07.core
  (:require [clojure.string :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.math.combinatorics :as combo]
            [clojure.walk :as w]
            [clojure.test :refer [deftest is run-tests]]))

(def test-input "resources/year2024/day07/test-input")
(def puzzle-input "resources/year2024/day07/input")

(defn parse-input
  [input]
  (->> (-> (slurp input) (s/split-lines))
       (map #(s/replace % #":" ""))
       (map #(s/split % #" "))
       (map #(map parse-long %))))

;; Part 1

(def part01-operators [:add :mult])

(defn infix->tree
  "Convert seq of operands and operators from infix notation to a tree so
      we can evaluate using postwalk. Operator precendence is not considered.

      1 * 2 + 3 + 4 =>
      [1 :mult 2 :plus 3 :plus 4] =>
      [[:add [:mult 1 2] 3] :add 4]"
  [infix-expr]
  (loop [expr infix-expr
         iter-count 1000]
    (when (zero? iter-count) (throw (Exception. "Iteration limit exceeded!")))
    (let [[op1 oper op2 & rest] expr]
      (if (nil? rest)
        [oper op1 op2]
        (recur
         (vec (cons [oper op1 op2] rest))
         (dec iter-count))))))

(defn eval-tree
  [tree]
  (w/postwalk
   (fn [x]
     (if (vector? x)
       (let [[oper op1 op2] x]
         (condp = oper
           :mult   (* op1 op2)
           :add    (+ op1 op2)
           :concat (->> [op1 op2]     ; Ugh
                        (map str)
                        (apply str)
                        parse-long)))
       x))
   tree))

(defn solve-equation
  "Given a total and operands, try out all combinations of operands and see
   whether any work."
  [operators equation]
  (let [total (first equation)
        operands (rest equation)]
    (->> (apply combo/cartesian-product
                (drop-last 1 (interleave (map vector operands) (repeat operators))))
         (map #(zipmap [:total :operands :result] [total operands (->> % infix->tree eval-tree)]))
         (filter #(= (:total %) (:result %))))))

(defn solve
  [operators input]
  (->> input
       (map (partial solve-equation operators))
       (filter seq)
       (map first)
       (map :total)
       (apply +)))

(defn part01
  [input]
  (solve part01-operators (parse-input input))
  )

(deftest test-part01
  (is (= 3749 (part01 test-input)))
  (is (= 1545311493300 (part01 puzzle-input))))

;; Part 2

(def part02-operators [:mult :add :concat])

(defn part02
  [input]
  (solve part02-operators (parse-input input))
  )

(deftest test-part02
  (is (= 11387 (part02 test-input)))
  (is (= 169122112716571 (part02 puzzle-input))))

(run-tests)
