(ns year2023.day03.core
  (:require [clojure.string :as s]
            [clojure.test :refer [deftest is]]))

;; Input data

(def test-input (slurp "resources/year2023/day03/test-input"))
(def puzzle-input (slurp "resources/year2023/day03/input"))

(def test-schematic (-> test-input s/split-lines))
(def puzzle-schematic (-> puzzle-input s/split-lines))

;; Convert an input line into a hashmap with keys `:start-index` for the
;; index into the line where the part number given by `:num` starts. We also
;; keep the length of the number in `:len` for later calculations.

;; (map nums-in-line test-schematic)
;; => ([{:start-index 0, :num 467, :len 3} {:start-index 5, :num 114, :len 3}]
;;     []
;;     [{:start-index 2, :num 35, :len 2} {:start-index 6, :num 633, :len 3}]
;;     []
;;     [{:start-index 0, :num 617, :len 3}]
;;     [{:start-index 7, :num 58, :len 2}]
;;     [{:start-index 2, :num 592, :len 3}]
;;     [{:start-index 6, :num 755, :len 3}]
;;     []
;;     [{:start-index 1, :num 664, :len 3} {:start-index 5, :num 598, :len 3}])

(defn nums-in-line
  "Create seq of numbers found in a line and their start index."
  [line]
  (let [matches (re-seq #"\d+" line)]
    (first
     (reduce
      (fn [[result ndx] match]
        (let [new-ndx (s/index-of line match ndx)]
          [(conj result {:start-index new-ndx
                         :num (parse-long match)
                         :len (count match)}) (+ new-ndx (count match))]))
      [[] 0]
      matches))))

(deftest test-nums-in-line
  (let [line "467..114.."
        nums (nums-in-line line)]
    (is (= nums [{:start-index 0 :num 467 :len 3}
                 {:start-index 5 :num 114 :len 3}])))
  (let [line "114..114.."
        nums (nums-in-line line)]
    (is (= nums [{:start-index 0 :num 114 :len 3}
                 {:start-index 5 :num 114 :len 3}]))))


;; Determine if the element in `schematic` at `[x y]` coordinates is a symbol.

(defn is-symbol?
  "Return true if the element at coordinates `[x y]` in the schematic is a symbol."
  [schematic [x y]]
  (let [width (count (first schematic))
        elt (str (nth (s/join schematic) (+ x (* y width))))]
    (not (contains? #{"1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "."} elt))))

(deftest test-is-symbol?
  (is (is-symbol? test-schematic [3 1]))
  (is (not (is-symbol? test-schematic [0 0]))))

;; Take a schematic and return a sequence of part numbers and their coordinates
;; on the schematic.

(defn schematic->part-nos
  "Convert a schematic to seq of part numbers."
  [schematic]
  (->> schematic
       (map nums-in-line)
       (map-indexed (fn [line-no nums]
                      (map #(hash-map :coord [(:start-index %) line-no]
                                      :part-no (:num %)
                                      :len (:len %)) nums)))
       (apply concat)))

;; Return a list of adjacent points on a line. We break this into an upper
;; section, lower section and end caps. For example a line of length two
;; is enclosed by its adjacent points U, C and L:
;;
;; U U U U
;; C - - C
;; L L L L
;;
;; We remove points that fall outside of the boundary of the schematic.

(defn adjacent-points
  "Return seq of adjacent points to a line of length `len` starting at `[x y]`."
  [schematic [x y] len]
  (let [max-x (count (first schematic))
        max-y (count schematic)]
    (->>
     ;; Create list of points around the line
     (concat
      (map vector (range (dec x) (+ x len 1)) (repeat (dec y))) ; upper
      (map vector (range (dec x) (+ x len 1)) (repeat (inc y))) ; lower
      [[(dec x) y] [(+ x len) y]])                              ; endcaps
     ;; Filter out negative points
     (remove (fn [[x y]] (or (< x 0) (< y 0))))
     ;; Filter out points beyond schematic length and width
     (remove (fn [[x y]] (or (>= x max-x) (>= y max-y)))))))

(deftest test-adjacent-points
  (is (= (set (adjacent-points test-schematic [2 2] 2))
         (set (concat
               [[1 1] [2 1] [3 1] [4 1]]
               [[1 3] [2 3] [3 3] [4 3]]
               [[1 2] [4 2]])))
      (is (= (set (adjacent-points test-schematic [0 0] 1))
             (set [[0 1]
                   [1 1]
                   [1 0]])))))

;; Part 1

(defn part-no-valid?
  "Part is valid if it has at least one adjacent point that is a symbol."
  [schematic part]
  (some identity
        (map (partial is-symbol? schematic)
             (adjacent-points schematic (:coord part) (:len part)))))

(deftest test-part-no-valid?
  (let [schematic test-schematic
        parts (schematic->part-nos schematic)]
    (is (part-no-valid? schematic (first parts)))
    (is (not (part-no-valid? schematic (second parts))))))

(comment

  ;; Determine sum of valid part numbers

  (let [schematic puzzle-schematic
        part-nos (schematic->part-nos schematic)]
    (->> part-nos
         (filter (partial part-no-valid? schematic))
         (map :part-no)
         (reduce + 0))) ;; => 549908

  :rcf)

;; Part 2

(defn is-symbol-star?
  "Return true if the element at coordinates `[x y]` in the schematic is a symbol."
  [schematic [x y]]
  (let [width (count (first schematic))
        elt (str (nth (s/join schematic) (+ x (* y width))))]
    (= elt "*")))

(deftest test-is-symbol-star
  (let [schematic test-schematic]
    (is (is-symbol-star? schematic [3 1]))
    (is (not (is-symbol-star? schematic [0 0])))))

(defn stars
  "Return a seq of all star symbols in a schematic."
  [schematic]
  (let [width (count (first schematic))
        height (count schematic)]
    (filter
     (partial is-symbol-star? schematic)
     (for [x (range width) y (range height)] (vector x y)))))

(deftest test-stars
  (is (= (set (stars test-schematic))
         (set [[3 1] [3 4] [5 8]]))))

;; Gears are defined as star symbols that are adjacent to exactly
;; two parts.

(defn gears
  "Return gears for a schematic."
  [schematic]
  (let [parts-with-ap (map #(assoc % :adjacent-points (adjacent-points schematic (:coord %) (:len %)))
                           (schematic->part-nos schematic))]
    (->> (stars schematic)
         (map
          (fn [star]
            (let [gears (filter #(contains? (set (:adjacent-points %)) star) parts-with-ap)]
              (when (= (count gears) 2)
                {:star star :gears gears}))))
         (filter some?))))

(comment
  
  ;; Multiply the part numbers of each gear to calculate the gear
  ;; ratio, then sum the ratios of all gears.

  (->> (gears puzzle-schematic)
       (map :gears)
       (map (fn [[g1 g2]] (* (:part-no g1) (:part-no g2))))
       (reduce + 0)) ;; => 81166799

  :rcf)
