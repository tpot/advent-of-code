(ns year2024.day05.core
  (:require [clojure.string :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.test :refer [deftest is run-tests]]))

(def test-input "resources/year2024/day05/test-input")
(def puzzle-input "resources/year2024/day05/input")

;; Input parsing

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

;; Part 1

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
  "Return true if an update has pages in order, false if it violates one or
   more rules, nil if no rules were relevant."
  [rules update]
  (reduce
   (fn [result rule]
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

;; Part 2

(defn in-order
  "Put an update in order according to rules"
  [rules update]

  ; Loop around until the update is in order or we blow our iteration budge
  (loop [new-update (vec update)
         max-iter 1000]
    (cond
      ; Done too much work - boom
      (zero? max-iter) (throw (Exception. "Iteration limit exceeded"))

      ; Update is in order - nothing else to do
      (in-order? rules new-update)
      new-update

      ; Go through all the rules and when we find a rule that fails, swap
      ; the values around and retry
      :else
      (recur
       (reduce
        (fn [result {:keys [antecedent dependent]}]
          (let [index-a (index-of result antecedent)
                index-b (index-of result dependent)]
            (if (and index-a index-b (>= index-a index-b))
              ; Rule fails
              (let [val-a (get result index-a)
                    val-b (get result index-b)]
                (-> result
                    (assoc index-a val-b)
                    (assoc index-b val-a)))
              ; Rule succeeds
              result)))
        new-update
        rules)
       (dec max-iter)))))

(defn part02
  [input]
  (let [{:keys [rules updates]} (parse-input input)]
    (->> updates
         (remove (partial in-order? rules))
         (map (partial in-order rules))
         (map middle)
         (apply +))))

(deftest test-part02
  (is (= 123 (part02 test-input)))
  (is (= 4944 (part02 puzzle-input))))

(run-tests)
