(ns year2016.day05.core
  (:require [clojure.string :as str]
            [clj-commons.digest :as digest]
            [clojure.test :refer [deftest is run-tests]]))

(def puzzle-input "ojvtpuvg")

(defn door-hash-seq
  "Lazy sequence of MD5 hash of door ID and an increasing integer."
  [door-id]
  (map
   #(digest/md5 (str/join [%1 %2]))
   (repeat door-id)
   (range)))

(defn part01
  [door-id]
  (->> (into []
             (comp
              (filter #(str/starts-with? % "00000"))
              (map #(nth % 5))
              (take 8))
             (door-hash-seq door-id))
       (map str)
       (apply str)))

(deftest test-part01
  (is (= "18f47a30" (part01 "abc")))
  (is (= "4543c154" (part01 puzzle-input))))

(defn part02
  [input])

(deftest test-part02)

(run-tests)
