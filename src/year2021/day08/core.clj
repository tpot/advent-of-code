(ns year2021.day08.core
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [clojure.pprint :refer [pprint]]
            [clojure.test :refer [deftest is run-tests]]))

;; Input parsing

(def test-input "resources/year2021/day08/test-input")
(def puzzle-input "resources/year2021/day08/input")

(defn str->kwset
  "Transform each character in a string to a set of keywords"
  [x]
  (->> x (map #(-> % str keyword)) set))

(defn parse-line
  [line]
  (let [[signals output] (s/split line #" \| ")]
    (->> [signals output]
         (map #(map str->kwset (s/split % #" ")))
         (zipmap [:signals :output]))))

(defn parse-input
  [input]
  (map parse-line (-> (slurp input) s/split-lines)))

;; Part 1

(defn process-reading-part01
  [reading]
  (->> (get reading :output)
       (map count)))

(defn part01
  [input]
  (let [readings (parse-input input)
        counts (update-vals
                (->> readings
                     (map process-reading-part01)
                     flatten
                     (group-by identity))
                count)]
    ; Digit usage counts:
    ;
    ; 1   [:c :f]
    ; 4   [:b :c :d :f]
    ; 7   [:a :c :f]
    ; 8   [:a :b :c :d :e :f :g]
    (reduce + 0 (map #(get counts %) [2 3 4 7]))))

(deftest test-part01
  (is (= 26 (part01 test-input)))
  (is (= 412 (part01 puzzle-input))))

;; Part 2

(def segments->digit
  (update-keys
   {[:a :b :c    :e :f :g] 0
    [:c             :f] 1
    [:a    :c :d :e    :g] 2
    [:a    :c :d    :f :g] 3
    [:b :c    :d    :f] 4
    [:a :b    :d    :f :g] 5
    [:a :b    :d :e :f :g] 6
    [:a    :c       :f] 7
    [:a :b :c :d :e :f :g] 8
    [:a :b :c :d    :f :g] 9}
   set))

(defn signals->mapping
  "Determine the mapping of signals for a single reading"
  [signals]
  (let [known-digits
        (->> [[1 2] [4 4] [7 3]]
             (map
              (fn [[digit num-segments]]
                [digit (->> signals (filter #(= num-segments (count %))) first)]))
             (into {}))
        ; Signals for known digits
        one-digit (get known-digits 1)
        four-digit (get known-digits 4)
        seven-digit (get known-digits 7)
        ; Mapping for :a will be the element that isn't in a subet for digit 1
        a (first (set/difference seven-digit one-digit))
        ; Take the signal for four and add the known mapping for :a. The
        ; signal that is a subset of that which is also 6 digits. What's
        ; leftover is the mapping for :g
        g (first (set/difference
                  (->> signals
                       (filter #(set/subset? (conj four-digit a) %))
                       (filter #(= 6 (count %)))
                       first)
                  (conj four-digit a)))
        ; Do the same trick for 7 + mapping for :g which yields the mapping
        ; for :d
        d (first (set/difference
                  (->> signals
                       (filter #(set/subset? (conj seven-digit g) %))
                       (filter #(= 5 (count %)))
                       first)
                  (conj seven-digit g)))
        b (first (set/difference
                  (->> signals
                       (filter #(set/subset? (set/union one-digit #{a d g}) %))
                       (filter #(= 6 (count %)))
                       first)
                  (set/union one-digit #{a d g})))
        f (first (set/difference
                  (->> signals
                       (filter #(set/subset? #{a d g b} %))
                       (filter #(= 5 (count %)))
                       first)
                  #{a d g b}))
        c (first (set/difference one-digit #{f}))
        e (first (set/difference #{:a :b :c :d :e :f :g} #{a b c d f g}))]
    (zipmap [a b c d e f g] [:a :b :c :d :e :f :g])))

(defn signal->digit
  "Map a signal to a digit"
  [mapping digit]
  (->> digit
       (map #(get mapping %))
       set
       segments->digit))

(defn output->number
  "Map outputs to a four digit number"
  [mapping output]
  (reduce
   (fn [result input] (+ input (* 10 result)))
   0
   (map (partial signal->digit mapping) output)))

(defn part02
  [input]
  (->> (parse-input input)
       (map
        (fn [{:keys [signals output]}]
          (output->number (signals->mapping signals) output)))
       (reduce + 0)))

(deftest test-part02
  (is (= 61229 (part02 test-input)))
  (is (= 978171 (part02 puzzle-input))))

(run-tests)
