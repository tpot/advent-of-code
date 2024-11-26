(ns year2023.day05.core
  (:require [clojure.string :as s]
            [clojure.test :refer [deftest is]]))

(def test-input (slurp "resources/year2023/day05/test-input"))
(def puzzle-input (slurp "resources/year2023/day05/input"))

;; Parse input

(defn parse-block
  "Parse a double-newline separated paragraph."
  [s]
  (let [[map-type-str num-list-str] (s/split s #"( map)?:\s")
        num-lists (vec (map #(->> (s/split % #" ") (map parse-long) vec)
                            (s/split-lines num-list-str)))]
    [(keyword map-type-str) num-lists]))

(defn input->almanac
  "Parse an input buffer into an almanac."
  [input]
  (->> (-> input (s/split #"\n\n"))
       (map parse-block)
       (into {})))

(defn src->dest
  "Map number `n` with maps specified according to the puzzle."
  [src-dest-maps n]
  (reduce
   (fn [_ [dst-range-start src-range-start range-len]]
     (if (and (>= n src-range-start)
              (<= n (dec (+ src-range-start range-len))))
       (reduced (+ (- n src-range-start) dst-range-start))
       n))
   nil src-dest-maps))

(deftest test-src->dest
  (is (= 50 (src->dest [[50 98 2]] 98)))
  (is (= 51 (src->dest [[50 98 2]] 99)))
  (is (= 100 (src->dest [[50 98 2]] 100))))

(defn seed->soil            [almanac n] (src->dest (get almanac :seed-to-soil)            n))
(defn soil->fertilizer      [almanac n] (src->dest (get almanac :soil-to-fertilizer)      n))
(defn fertilizer->water     [almanac n] (src->dest (get almanac :fertilizer-to-water)     n))
(defn water->light          [almanac n] (src->dest (get almanac :water-to-light)          n))
(defn light->temperature    [almanac n] (src->dest (get almanac :light-to-temperature)    n))
(defn temperature->humidity [almanac n] (src->dest (get almanac :temperature-to-humidity) n))
(defn humidity->location    [almanac n] (src->dest (get almanac :humidity-to-location)    n))

(defn seed->location
  [almanac seed]
  (->> seed
       (seed->soil almanac)
       (soil->fertilizer almanac)
       (fertilizer->water almanac)
       (water->light almanac)
       (light->temperature almanac)
       (temperature->humidity almanac)
       (humidity->location almanac)))

;; Part 1

(defn part01
  [input]
  (apply min
   (let [almanac (input->almanac input)
         seeds (first (get almanac :seeds))]
     (map (partial seed->location almanac) seeds))))

(deftest test-part01
  (is (= 35 (part01 test-input)))
  (is (= 278755257 (part01 puzzle-input))))

;; Part 2

(comment

  (defn seeds-from-seed-ranges
    [seed-ranges]
    (mapcat
     (fn [[start count]] (range start (+ start count)))
     (partition 2 seed-ranges)))

  (deftest test-seeds-from-seed-ranges
    (is (= 27 (count (seeds-from-seed-ranges [79 14 55 13])))))

  (apply min
    (let [almanac (input->almanac puzzle-input)
          seeds (seeds-from-seed-ranges (first (get almanac :seeds)))]
      (map (partial seed->location almanac) seeds)))

  :rcf)
