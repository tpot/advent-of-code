(ns year2016.day04.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is run-tests]]))

(def puzzle-input "resources/year2016/day04/input")

(defn parse-input
  [input]
  (->> (line-seq (clojure.java.io/reader input))
       (map #(re-matches #"([a-z-]+)-(\d+)\[(.*)\]" %))
       (map #(zipmap [:name :sector-id :checksum] (drop 1 %)))
       (map #(update % :sector-id parse-long))))

(defn letter-freq
  "Return map of letter frequency for a room name."
  [s]
  (-> (group-by str s)
      (update-vals count)
      (dissoc "-")))

(defn checksum
  "Return checksum for string."
  [s]
  (let [lf (letter-freq s)]
    (->> (keys lf)
         (sort
          #(let [a (-> %1 first int)
                 b (-> %2 first int)
                 a-freq (get lf %1)
                 b-freq (get lf %2)]
             (if (= a-freq b-freq)
               (< a b)
               (> a-freq b-freq))))
         (take 5)
         str/join)))

(deftest test-checksum
  (is (= "abxyz" (checksum "aaaaa-bbb-z-y-x")))
  (is (= "abcde" (checksum "a-b-c-d-e-f-g-h")))
  (is (= "oarel" (checksum "not-a-real-room"))))

(defn part01
  [input]
  (->> input
       (filter #(= (get % :checksum) (checksum (get % :name))))
       (map :sector-id)
       (apply +)))

(deftest test-part01
  (is (= 173787 (part01 (parse-input puzzle-input)))))

(def INT-A (int (first "a")))

(defn rotate-char
  "Rotate alphabetic char c by n digits"
  [n c]
  (if (= c \-)
    \space
    (-> c
        int
        (- INT-A)
        (+ n)
        (mod 26)
        (+ INT-A)
        char)))

(defn rotate-string
  "Rotate alphabetic string s by n digits"
  [n s]
  (->> s (map (partial rotate-char n)) str/join))

(deftest test-rotate-string
  (is (= "very encrypted name" (rotate-string 343 "qzmt-zixmtkozy-ivhz"))))

(defn part02
  [input]
  (->> input
       (filter #(= "northpole object storage" (rotate-string (get % :sector-id) (get % :name))))
       first
       :sector-id))

(deftest test-part02
  (is (= 548 (part02 (parse-input puzzle-input)))))

(run-tests)
