(ns year2024.day11.core
  (:require [clojure.string :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.test :refer [deftest is run-tests]]))

(def test-file "resources/year2024/day11/test-input")
(def puzzle-file "resources/year2024/day11/input")

;; Input parsing

(defn parse-input
  [file]
  (->> (-> (slurp file) s/trim (s/split #" "))
       (map parse-long)))

;; Part 1

(defn iter
  [input iter-count]
  (loop [xs input
         max-iters iter-count]
    (if (zero? max-iters) xs
        (recur
         (mapcat
          #(cond
             ;; Rule 1
             (= % 0)
             [1]

             ;; Rule 2
             (= (mod (count (str %)) 2) 0)
             [(parse-long (subs (str %) 0 (/ (count (str %)) 2)))
              (parse-long (subs (str %)   (/ (count (str %)) 2)))]

             ;; When rule 1 and rule 2 do not apply
             :else
             [(* % 2024)])
          xs)
         (dec max-iters)))))

(defn part01
  [file iter-count]
  (count (iter (parse-input file) iter-count)))

(deftest test-part01
  (is (= 55312 (part01 test-file 25)))
  (is (= 216996 (part01 puzzle-file 25))))

;; Part 2

(defn part02
  [file]
  )

(deftest test-part02
  )

(run-tests)
