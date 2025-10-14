(ns year2016.day09.core
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.test :refer [deftest is run-tests]]))

(def puzzle-input "resources/year2016/day09/input")

(defn parse-input
  [input]
  (->> input
       (re-seq #"[A-Z]|\((\d+)x(\d+)\)")
       (map #(if (nil? (second %))
               (first %)
               (let [len (second %)
                     count (nth % 2)]
                 {:len (parse-long len) :count (parse-long count)})))))

(deftest test-parse-input
  (is (= (seq ["A" "D" "V" "E" "N" "T"]) (parse-input "ADVENT")))
  (is (= (seq ["A" {:len 1, :count 5} "B" "C"]) (parse-input "A(1x5)BC"))))

(defn xf-decompress [xf]
  (let [state (atom {:state-name :single-char})]
    (fn
      ([] (xf))
      ([result input]
       (condp = (get @state :state-name)
         ;; Spit out a single character unless we see a marker
         :single-char
         (cond
           ;; Single char
           (string? input)
           (xf result input)
           ;; Marker
           (map? input)
           (do
             (reset! state (merge {:state-name :decompress :str ""} input))
             result))
         ;; Process a compression marker
         :decompress
         (cond
           ;; Single char
           (string? input)
           (do
             ;; Collect char
             (swap! state update :len dec)
             (swap! state update :str #(str % input))
             ;; Saw final char?
             (if (= (get @state :len) 0)
               ;; Spit out string
               (let [count (get @state :count)
                     s (get @state :str)]
                 (reset! state {:state-name :single-char})
                 (xf result (apply str (repeat count s))))
               ;; Keep going
               result))
           ;; Nested marker
           (map? input)
           (let [s (format "(%dx%d)" (get input :len) (get input :count))]
             (swap! state update :len #(- % (count s)))
             (swap! state update :str #(str % s))
             result))))
      ([result] (xf result)))))

(deftest test-xf-decompress
  (is (= "ABC"                (transduce xf-decompress str "" ["A" "B" "C"])))
  (is (= "ABCBCD"             (transduce xf-decompress str "" ["A" {:len 2 :count 2} "B" "C" "D"])))
  (is (= "XYZXYZXYZ"          (transduce xf-decompress str "" (parse-input "(3x3)XYZ"))))
  (is (= "ABCBCDEFEFG"        (transduce xf-decompress str "" (parse-input "A(2x2)BCD(2x2)EFG"))))
  (is (= "(1x3)A"             (transduce xf-decompress str "" (parse-input "(6x1)(1x3)A"))))
  (is (= "X(3x3)ABC(3x3)ABCY" (transduce xf-decompress str "" (parse-input "X(8x2)(3x3)ABCY")))))

(defn part01
  [input]
  (count (transduce xf-decompress str "" input)))

(deftest test-part01
  (is (= 98135 (part01 (parse-input (slurp puzzle-input))))))

(defn part02
  [input])

(deftest test-part02)

(run-tests)
