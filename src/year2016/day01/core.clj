(ns year2016.day01.core
  (:require [clojure.string :as s]
            [clojure.test :refer [deftest is run-tests]]
            [clojure.java.io :as io]))

(def puzzle-input "resources/year2016/day01/input")

(defn parse-input
  "Return lazy seq of instructions from a comma-separated input file."
  [input]
  (->> (line-seq (clojure.java.io/reader input))
       (mapcat #(s/split % #", "))
       (map #(vector (keyword (str (first %))) (parse-long (subs % 1))))))

(deftest test-parse-input
  (let [input (io/reader (java.io.StringReader. "L1, R2, L3, R4"))
        expected (seq [[:L 1] [:R 2] [:L 3] [:R 4]])]
    (is (= expected (parse-input input)))))

;; Compass points and a bidirectional lookup table

(def COMPASS-POINTS [:N :E :S :W])

(def COMPASS-POINTS-LOOKUP
  (let [compass-points-list (map-indexed vector COMPASS-POINTS)]
    (into {}
          (concat
           compass-points-list
           (->> compass-points-list (map reverse) (map vec))))))

(defn new-facing-dir
  "Return new facing direction given current facing and turn direction"
  [facing turn-dir]
  (let [new-facing (condp = turn-dir
                         :R (inc (get COMPASS-POINTS-LOOKUP facing))
                         :L (dec (get COMPASS-POINTS-LOOKUP facing)))]
    (get COMPASS-POINTS-LOOKUP (mod new-facing 4))))

(defn new-pos
  "Calculate new position after processing directions"
  [input]
  (reduce
   (fn [state [turn-dir dist]]
     (let [new-facing (new-facing-dir (get state :facing) turn-dir)
           {:keys [x-pos y-pos]} state]
       {:facing new-facing
        :x-pos (condp = new-facing
                 :E (+ x-pos dist)
                 :W (- x-pos dist)
                 x-pos)
        :y-pos (condp = new-facing
                 :N (+ y-pos dist)
                 :S (- y-pos dist)
                 y-pos)}))
   {:facing :N :x-pos 0 :y-pos 0}
   input))

(defn rdr [input] (io/reader (java.io.StringReader. input)))

(deftest test-new-pos
  (is (= {:facing :N :x-pos 2  :y-pos 3}  (new-pos (parse-input (rdr "R2, L3")))))
  (is (= {:facing :W :x-pos 0  :y-pos -2} (new-pos (parse-input (rdr "R2, R2, R2")))))
  (is (= {:facing :S :x-pos 10 :y-pos 2}  (new-pos (parse-input (rdr "R5, L5, R5, R3"))))))

(defn part01
  [input]
  (let [pos (new-pos input)
        {:keys [x-pos y-pos]} pos]
    (+ (abs x-pos) (abs y-pos))))

(deftest test-part01
  (is (= 5   (part01 (parse-input (rdr "R2, L3")))))
  (is (= 2   (part01 (parse-input (rdr "R2, R2, R2")))))
  (is (= 12  (part01 (parse-input (rdr "R5, L5, R5, R3")))))
  (is (= 291 (part01 (parse-input puzzle-input)))))

(defn part02
  [input])

(deftest test-part02)

(run-tests)
