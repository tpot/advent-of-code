(ns year2021.day10.core
  (:require [clojure.string :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.test :refer [deftest is run-tests]]))

;; Input parsing

(def test-input "resources/year2021/day10/test-input")
(def puzzle-input "resources/year2021/day10/input")

(def nav-chars
  [["(" :open-paren]
   [")" :close-paren]
   ["[" :open-square]
   ["]" :close-square]
   ["{" :open-curly]
   ["}" :close-curly]
   ["<" :open-angle]
   [">" :close-angle]])

(def char-map (into {} nav-chars))

(def matching-char
  [[:open-paren  :close-paren]
   [:open-square :close-square]
   [:open-curly  :close-curly]
   [:open-angle  :close-angle]])

(def matching-char-map (into {} matching-char))

(defn parse-line
  [line]
  (->> line (map str) (map char-map)))

(defn parse-input
  [input]
  (map parse-line (-> (slurp input) s/split-lines)))

;; Part 1

(defn open-char?
  "Return true if char is an open character"
  [x]
  (contains? #{:open-paren :open-square :open-curly :open-angle} x))

(defn close-char?
  "Return true if char is a close character"
  [x]
  (contains? #{:close-paren :close-square :close-curly :close-angle} x))

(defn matching-close?
  "Return true if close-char matches open-char"
  [open-char close-char]
  (= (get matching-char-map open-char) close-char))

(def incorrect-char-cost
  [[:close-paren  3]
   [:close-square 57]
   [:close-curly  1197]
   [:close-angle  25137]])

(def incorrect-char-cost-map (into {} incorrect-char-cost))

(defn find-incorrect-char
  [line]
  (loop [nav-sub line
         block-stack ()
         max-iter 1000]
    (cond
      (zero? max-iter) (throw (Exception. "Iteration limit exceeded!"))
      (empty? line) true
      :else
      (let [block (first nav-sub)]
        (if (open-char? block)
          (recur
           (rest nav-sub)
           (conj block-stack block)
           (dec max-iter))
          (if (not (matching-close? (first block-stack) block))
            block
            (recur
             (rest nav-sub)
             (rest block-stack)
             (dec max-iter))))))))

(defn part01
  [input]
  (let [lines (parse-input input)]
    (->> lines
         (map find-incorrect-char)
         (remove nil?)
         (map incorrect-char-cost-map)
         (reduce + 0))))

(deftest test-part01
  (is (= 26397 (part01 test-input)))
  (is (= 323613 (part01 puzzle-input))))

;; Part 2

(defn part02
  [input])

(deftest test-part02)

(run-tests)
