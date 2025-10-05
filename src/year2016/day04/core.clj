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

(defn part02
  [input])

(deftest test-part02)

(run-tests)
