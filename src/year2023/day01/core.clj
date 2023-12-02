(ns year2023.day01.core
  (:require [clojure.string :as s]))

(def puzzle-input (-> (slurp "resources/year2023/day01/input") s/split-lines))

;; Part 1

(defn calibration-value
  [x]
  (->> (re-seq #"\d" x)
       (#(vector (first %) (last %)))
       s/join
       parse-long))

(comment

  (def test-input ["1abc2"
                   "pqr3stu8vwx"
                   "a1b2c3d4e5f"
                   "treb7uchet"])

  (reduce + 0 (map calibration-value test-input)) ;; => 142
  (reduce + 0 (map calibration-value puzzle-input)) ;; => 55090

  :rcf)

;; Part 2

(def DIGITS ["1" "2" "3" "4" "5" "6" "7" "8" "9"])
(def LETTER-DIGITS ["one" "two" "three" "four" "five" "six" "seven" "eight" "nine"])
(def LETTER-DIGITS-MAP (merge (zipmap LETTER-DIGITS DIGITS) (zipmap DIGITS DIGITS)))

(defn indexes-for
  "Return indexes for an alphabet of substrings found in an input string searching from the start of the input."
  [alphabet input]
  (->> (map (partial s/index-of input) alphabet)
       (zipmap alphabet)
       (into [])
       (remove #(nil? (second %)))
       (sort-by second)))

(defn last-indexes-for
  "Return indexes for an alphabet of substrings found in an input string searching from the end of the input."
  [alphabet input]
  (->> (map (partial s/last-index-of input) alphabet)
       (zipmap alphabet)
       (into [])
       (remove #(nil? (second %)))
       (sort-by second)))

(defn calibration-value2
  [input]
  (let [indexes (sort-by second
                         (concat (indexes-for DIGITS input)
                                 (indexes-for LETTER-DIGITS input)
                                 (last-indexes-for DIGITS input)
                                 (last-indexes-for LETTER-DIGITS input)))]
    (->> [(first indexes) (last indexes)]
         (map first)
         (map #(get LETTER-DIGITS-MAP %))
         s/join
         parse-long)))

(comment

  (def test-input
    ["two1nine"
     "eightwothree"
     "abcone2threexyz"
     "xtwone3four"
     "4nineeightseven2"
     "zoneight234"
     "7pqrstsixteen"])

  (reduce + 0 (map calibration-value2 test-input)) ;; => 281
  (reduce + 0 (map calibration-value2 puzzle-input)) ;; => 54845

  :rcf)