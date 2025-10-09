(ns year2016.day07.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is run-tests]]))

(def puzzle-input "resources/year2016/day07/input")

(defn parse-address
  [s]
  (let [supernets (-> s (str/replace #"\[[a-z]+\]" "[]") (str/split #"\[\]"))
        hypers (->> s (re-seq #"\[([a-z]+)\]") (mapv second))]
    {:supernets supernets :hypers hypers})
  )

(deftest test-parse-address
  (is (= {:supernets ["abba" "qrst"], :hypers ["mnop"]} (parse-address "abba[mnop]qrst"))))

(defn parse-input
  [input]
  (->> (line-seq (clojure.java.io/reader input))
       (map parse-address)))

(defn abba?
  "Return true if string is in ABBA format."
  [[a b c d]]
  (and
   ;; First/last, middle two are equal
   (= a d) (= b c)
   ;; But can't all be the same
   (not (= a b))))

(defn addr-abba?
  "Do any sequences of four letters match ABBA format?"
  [addr]
  (some abba? (partition 4 1 addr)))

(deftest test-addr-abba
  (is (abba? "abba"))
  (is (not (abba? "abab")))
  (is (not (abba? "aaaa"))))

(defn supports-tls?
  "Does an address support TLS?"
  [{:keys [supernets hypers]}]
  (boolean
   (and
    ;; TLS is supported if least one of the addresses is ABBA
    (some true? (map addr-abba? supernets))
    ;; But not if any of the hyper addresses is ABBA
    (not (some true? (map addr-abba? hypers))))))

(deftest test-supports-tls
  (is (= true  (supports-tls? (parse-address "abba[mnop]qrst"))))
  (is (= false (supports-tls? (parse-address "abcd[bddb]xyyx"))))
  (is (= false (supports-tls? (parse-address "aaaa[qwer]tyui"))))
  (is (= true  (supports-tls? (parse-address "ioxxoj[asdfgh]zxcvbn")))))

(defn part01
  [input]
  (->> input
       (filter supports-tls?)
       count))

(deftest test-part01
  (is (= 110 (part01 (parse-input puzzle-input)))))

(defn part02
  [input]
)

(deftest test-part02
)

(run-tests)
