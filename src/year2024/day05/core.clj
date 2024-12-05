(ns year2024.day05.core
  (:require [clojure.string :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.test :refer [deftest is run-tests]]))

(def test-input "resources/year2024/day05/test-input")
(def puzzle-input "resources/year2024/day05/input")

(defn parse-input
  [input]
  (let [[rules _ updates] (->> (-> (slurp input) s/split-lines)
                               (partition-by #(= % ""))
                               vec)]
    {:rules   (->> rules
                   (map #(re-matches #"(\d+)\|(\d+)" %))
                   (map #(zipmap
                          [:antecedent :dependent]
                          [(parse-long (nth % 1)) (parse-long (nth % 2))])))
     :updates (->> updates (map #(s/split % #",")) (map #(map parse-long %)))}))

(defn index-of
  "Return index of element in a vector, or nil if not found"
  [v element]
  (first
   (keep-indexed (fn [i x] (when (= x element) i)) v)))

(defn elt-before
  "Return true if a comes before b in v, false otherwise"
  [v a b]
  (let [index-a (index-of v a)
        index-b (index-of v b)]
    (when (and index-a index-b)
      (< index-a index-b))))

(defn in-order?
  [rules update]
  (reduce
   (fn [result rule]
     #_(println "=>" update rule)
     (condp = (elt-before update (:antecedent rule) (:dependent rule))
           ; Rule does not apply
       nil result
       true true
       false (reduced false)))
   nil
   rules))

(defn middle [xs] (nth xs (/ (dec (count xs)) 2)))

(defn part01
  [input]
  (let [{:keys [rules updates]} (parse-input input)]
    (->> updates
         (filter (partial in-order? rules))
         (map middle)
         (apply +))))

(deftest test-part01
  (is (= 143 (part01 test-input)))
  (is (= 6612 (part01 puzzle-input))))

(defn part02
  [input])

(deftest test-part02)

(run-tests)
