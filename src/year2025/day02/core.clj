(ns year2025.day02.core
  (:require [clojure.string :as s]
            [clojure.math :as math]
            [clojure.test :refer [deftest is run-tests]]))

(def test-input "resources/year2025/day02/test-input")
(def puzzle-input "resources/year2025/day02/input")

(defn parse-input
  [path]
  (->> (-> (slurp path) s/trim (s/split #","))
       (map #(s/split % #"-"))
       (map #(vector (parse-long (first %)) (parse-long (second %))))))

(defn even-digits?
  "Return true if a number contains an even number of digits."
  [n]
  (-> n math/log10 int odd?))

(defn split-num
  "Split a number into two halves."
  [n]
  (let [split       (math/pow 10 (inc (math/floor-div (int (math/log10 n)) 2)))
        top-half    (math/floor-div n split)
        bottom-half (math/floor-mod n split)]
    [top-half bottom-half]))

(defn invalid-product-id?
  "Return true if a product ID is invalid"
  [n]
  (->> n split-num (apply =)))

(defn invalid-product-ids-in-range
  "Return all invalid product IDs between low and high, inclusive."
  [low high]
  (->> (range  low (inc high))
       (filter even-digits?)
       (filter invalid-product-id?)))

(deftest test-invalid-product-ids-in-range
  (is (= (seq [11 22]) (invalid-product-ids-in-range 11 22)))
  (is (= (seq [99])    (invalid-product-ids-in-range 95 115))))

(defn part01
  [input]
  (->> input
       (mapcat (fn [[low high]] (invalid-product-ids-in-range low high)))
       (apply +)))

(deftest test-part01
  (is (= 1227775554  (part01 (parse-input test-input))))
  (is (= 21139440284 (part01 (parse-input puzzle-input)))))

(defn part02
  [input]
)

(deftest test-part02
)

(run-tests)
