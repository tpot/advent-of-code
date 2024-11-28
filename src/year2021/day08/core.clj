(ns year2021.day08.core
  (:require [clojure.string :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.test :refer [deftest is run-tests]]))

;; Input parsing

(def test-input "resources/year2021/day08/test-input")
(def puzzle-input "resources/year2021/day08/input")

(defn str->kwset
  "Transform each character in a string to a set of keywords"
  [x]
  (->> x (map #(-> % str keyword)) set))

(defn parse-line
  [line]
  (let [[signals output] (s/split line #" \| ")]
    (->> [signals output]
         (map #(map str->kwset (s/split % #" ")))
         (zipmap [:signals :output]))))

(defn parse-input
  [input]
  (map parse-line (-> (slurp input) s/split-lines)))

;; Part 1

(defn process-reading-part01
  [reading]
  (->> (get reading :output)
       (map count)))

(defn part01
  [input]
  (let [readings (parse-input input)
        counts (update-vals
                (->> readings
                     (map process-reading-part01)
                     flatten
                     (group-by identity))
                count)]
    ; Digit usage counts:
    ;
    ; 1   [:c :f]
    ; 4   [:b :c :d :f]
    ; 7   [:a :c :f]
    ; 8   [:a :b :c :d :e :f :g]
    (reduce + 0 (map #(get counts %) [2 3 4 7]))))

(deftest test-part01
  (is (= 26 (part01 test-input)))
  (is (= 412 (part01 puzzle-input))))

;; Part 2

(defn part02
  [input])

(deftest test-part02)

(run-tests)
