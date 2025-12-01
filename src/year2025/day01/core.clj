(ns year2025.day01.core
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.test :refer [deftest is run-tests]]))

(def test-input "resources/year2025/day01/test-input")
(def puzzle-input "resources/year2025/day01/input")

(defn parse-line
  [line]
  [(keyword (subs line 0 1)) (parse-long (subs line 1))])

(defn parse-input
  [input]
  (->> (line-seq (clojure.java.io/reader input))
       (map parse-line)))

(defn apply-movements
  [xs]
  (reduce
   (fn [result [dir count]]
     (let [update-fn (if (= dir :L) - +)
           new-pos (-> (get result :pos) (update-fn count) (mod 100))]
       (-> result
           (assoc :pos new-pos)
           (update :zero-count
                   (fn [zero-count] (if (zero? new-pos) (inc zero-count) zero-count))))))
   {:pos 50 :zero-count 0}
   xs))

(deftest test-apply-movements
  (is (= {:pos 32 :zero-count 3}    (apply-movements (parse-input test-input))))
  (is (= {:pos 69 :zero-count 1105} (apply-movements (parse-input puzzle-input)))))

(defn part01
  [input]
  (-> (parse-input input)
      apply-movements
      :zero-count))

(deftest test-part01
  (is (= 3    (part01 test-input)))
  (is (= 1105 (part01 puzzle-input))))

(defn part02
  [input]
)

(deftest test-part02
)

(run-tests)
