(ns year2021.day02.core
  (:require [clojure.string :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.test :refer [deftest is run-tests]]))

(def test-input "resources/year2021/day02/test-input")
(def puzzle-input "resources/year2021/day02/input")

(defn parse-input
  [input]
  (->> (s/split-lines (slurp input))
       (map #(s/split % #" "))
       (map #(hash-map :cmd (keyword (first %)) :arg (parse-long (second %))))))

(defn part01
  [input]
  (let [{final-depth :depth, final-pos :pos}
        (reduce
         (fn [{:keys [depth pos] :as result} {:keys [cmd arg]}]
           (merge result
                  (condp = cmd
                    :forward {:pos   (+ pos arg)}
                    :down    {:depth (+ depth arg)}
                    :up      {:depth (- depth arg)})))
         {:depth 0 :pos 0}
         (parse-input input))]
    (* final-depth final-pos)))

(deftest test-part01
  (is (= 150 (part01 test-input)))
  (is (= 1990000 (part01 puzzle-input))))

(defn part02
  [input]
  (let [{final-depth :depth, final-pos :pos}
        (reduce
         (fn [{:keys [depth pos aim] :as result} {:keys [cmd arg]}]
           (merge result
                  (condp = cmd
                    :forward {:pos   (+ pos arg)
                              :depth (+ depth (* aim arg))}
                    :down    {:aim   (+ aim arg)}
                    :up      {:aim   (- aim arg)})))
         {:depth 0 :pos 0 :aim 0}
         (parse-input input))]
    (* final-depth final-pos)))

(deftest test-part02
  (is (= 900 (part02 test-input)))
  (is (= 1975421260 (part02 puzzle-input))))

(run-tests)
