(ns year2016.day01.core
  (:require [clojure.string :as s]
            [clojure.set :as set]
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

(defn path
  "Path between two endpoints."
  [x1 y1 x2 y2]
  (let [x-delta (- x2 x1)
        y-delta (- y2 y1)
        x-range (if (zero? x-delta)
                  (repeat x1)
                  (range x1 (if (< x2 x1) (dec x2) (inc x2)) (if (< x2 x1) -1 1)))
        y-range (if (zero? y-delta)
                  (repeat y1)
                  (range y1 (if (< y2 y1) (dec y2) (inc y2)) (if (< y2 y1) -1 1)))]
    (map vector x-range y-range)))

(defn get-first-visited
  "Return the first location from path that is present in visited."
  [visited path]
  (reduce
   (fn [_ loc]
     (if (contains? visited loc)
       (reduced loc)
       nil))
   nil
   path))

(defn new-pos
  "Calculate new position after processing directions"
  [input]
  (reduce
   (fn [state [turn-dir dist]]
     (let [new-facing (new-facing-dir (get state :facing) turn-dir)
           {:keys [x-pos y-pos visited-locations first-visited]} state
           new-x-pos (condp = new-facing
                       :E (+ x-pos dist)
                       :W (- x-pos dist)
                       x-pos)
           new-y-pos (condp = new-facing
                       :N (+ y-pos dist)
                       :S (- y-pos dist)
                       y-pos)
           path (path x-pos y-pos new-x-pos new-y-pos)
           loc (get-first-visited visited-locations (drop 1 path))]
       {:facing new-facing
        :x-pos new-x-pos
        :y-pos new-y-pos
        :visited-locations (set/union visited-locations (set path))
        :first-visited (if (and (nil? first-visited) loc) loc first-visited)}))
   {:facing :N :x-pos 0 :y-pos 0 :visited-locations #{} :first-visited nil}
   input))

(defn rdr [input] (io/reader (java.io.StringReader. input)))

(defn submap?
  "Return true if the keys in expected have the same values in actual."
  [expected actual]
  (= expected (select-keys actual (keys expected))))

(deftest test-new-pos
  (is (submap? {:facing :N :x-pos 2  :y-pos 3}  (new-pos (parse-input (rdr "R2, L3")))))
  (is (submap? {:facing :W :x-pos 0  :y-pos -2} (new-pos (parse-input (rdr "R2, R2, R2")))))
  (is (submap? {:facing :S :x-pos 10 :y-pos 2}  (new-pos (parse-input (rdr "R5, L5, R5, R3")))))
  (is (submap? {:first-visited [4 0]}           (new-pos (parse-input (rdr "R8, R4, R4, R8"))))))

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
  [input]
  (->> (new-pos input)
       :first-visited
       (apply +)))

(deftest test-part02
  (is (= 159 (part02 (parse-input puzzle-input) ))))

(run-tests)
