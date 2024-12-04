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

(defn process-nav-syntax
  "Process a line of navigation system syntax. Return "
  [line]
  (loop [nav-sub line
         block-stack ()
         max-iter 1000]
    (cond
      ; Loop termination conditions
      (zero? max-iter) (throw (Exception. "Iteration limit exceeded!"))
      (empty? nav-sub) (zipmap [:result :block-stack] [:success block-stack])

      :else
      (let [block (first nav-sub)]
        (cond
          ; Push new open char onto the stack
          (open-char? block)
          (recur
           (rest nav-sub)
           (conj block-stack block)
           (dec max-iter))

          ; Pop matching close char off the stack
          (matching-close? (first block-stack) block)
          (recur
           (rest nav-sub)
           (rest block-stack)
           (dec max-iter))

          ; Syntax error
          :else
          (zipmap
           [:result :illegal-character :block-stack]
           [:syntax-error block block-stack]))))))

(defn part01
  [input]
  (let [lines (parse-input input)]
    (->> lines
         (map process-nav-syntax)
         (filter #(= (get % :result) :syntax-error))
         (map :illegal-character)
         (map incorrect-char-cost-map)
         (apply +))))

(deftest test-part01
  (is (= 26397 (part01 test-input)))
  (is (= 323613 (part01 puzzle-input))))

;; Part 2

(def matching-char-cost
  [[:close-paren  1]
   [:close-square 2]
   [:close-curly  3]
   [:close-angle  4]])

(def matching-char-cost-map (into {} matching-char-cost))

(defn middle [xs] (nth xs (/ (dec (count xs)) 2)))

(defn part02
  [input]
  (let [lines (parse-input input)]
    (->> lines
         (map process-nav-syntax)
         (filter #(= (get % :result) :success))
         (map :block-stack)
         (map #(map matching-char-map %))
         (map #(map matching-char-cost-map %))
         (map #(reduce
                (fn [cost input]
                  (+ (* cost 5) input))
                0 %))
         (sort)
         (middle))))

(deftest test-part02
  (is (= 288957 (part02 test-input)))
  (is (= nil (part02 puzzle-input))))

(run-tests)
