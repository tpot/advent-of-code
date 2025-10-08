(ns year2016.day05.core
  (:require [clojure.string :as str]
            [clj-commons.digest :as digest]
            [clojure.test :refer [deftest is run-tests]]))

(def DEBUG true)
(def debug-prn (if DEBUG prn identity))

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
  (->>
   ;; Generate first eight hashes with five leading zeros
   (into []
         (comp
          (filter #(str/starts-with? % "00000"))
          (map #(doto % debug-prn))
          (take 8))
         (door-hash-seq door-id))
   ;; Take sixth position for each hash
   (map #(-> % (nth 5) str))
   ;; Return result as a string
   (map str)
   (apply str)))

(deftest test-part01
  (is (= "18f47a30" (part01 "abc")))
  (is (= "4543c154" (part01 puzzle-input))))

(def part02-xf
  "Transform elements of door-hash-seq into valid characters and positions."
  (comp
   ;; Filter hashes with five leading zeros
   (filter #(str/starts-with? % "00000"))
   ;; Convert to a hash of character and position
   (map #(hash-map :ch (str(nth % 6))
                   :pos (- (int (nth % 5)) (int \0))))
   ;; Keep only valid positions
   (filter #(let [c (get % :pos)]
              (and (>= c 0) (<= c 7))))
   (map #(doto % debug-prn))
   ))

(defn part02-rf-reduce
  "Collect the first eight valid characters and positions."
  [pos-chars {:keys [ch pos]}]
  (let [result (if (not (contains? pos-chars pos))
                     (assoc pos-chars pos ch)
                     pos-chars)]
    (if (= 8 (count (keys result)))
      (reduced result)
      result)))

(defn part02-rf-finalise
  "Finalise result when transduction complete."
  [result]
  (reduce
   (fn [code [pos ch]]
     (assoc code pos ch))
   (vec (take 8 (repeat "-")))
   result))

(defn part02
  [door-id]
  (transduce
   part02-xf
   (fn
     ([acc] (apply str (part02-rf-finalise acc)))    ; 1-arity finalise output when finished
     ([acc input] (part02-rf-reduce acc input)))     ; 2-arity processes each element
   nil
   (door-hash-seq door-id)))

(deftest test-part02
  (is (= "05ace8e3" (part02 "abc")))
  (is (= "1050cbbd" (part02 puzzle-input))))

(run-tests)
