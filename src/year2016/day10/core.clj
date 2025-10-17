(ns year2016.day10.core
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.test :refer [deftest is run-tests]]))

(def puzzle-input "resources/year2016/day10/input")

(defn parse-line
  [s]
  (condp re-matches s
    ;; Assign initial chips instruction
    #"^value (\d+) goes to bot (\d+)"
    :>> (fn [[_ value bot]]
          {:give-chip-value (parse-long value) :to-bot-num (parse-long bot)})
    ;; Move chips instructions
    #"^bot (\d+) gives low to (bot|output) (\d+) and high to (bot|output) (\d+)"
    :>> (fn [[_ bot gives-low-to low-dest gives-high-to high-dest]]
          {:bot-num (parse-long bot)
           :gives-low-to (keyword gives-low-to) :low-dest (parse-long low-dest)
           :gives-high-to (keyword gives-high-to) :high-dest (parse-long high-dest)})))

(defn parse-input
  [input]
  (->> (line-seq (clojure.java.io/reader input))
       (map parse-line)))

(defn assign-chip
  "Assign a chip to a destination, either :bot or :output are valid destinations."
  [state dest dest-num chip-value]
  (update-in state [dest dest-num] #(conj (or % #{}) chip-value)))

(defn drop-chip
  "Drop a chip from a destination by value."
  [state dest dest-num chip-value]
  (update-in state [dest dest-num] #(set/difference % #{chip-value})))

(defn get-chips
  "Return chips in a destination."
  [state dest dest-num]
  (or (get-in state [dest dest-num]) #{}))

(defn process-assign-chips
  "Update state with give chips instructions"
  [state input & {:keys [debug] :or {debug false}}]
  (reduce
   (fn [state instr]
     (when debug (println ">>>" instr))
     (let [{:keys [give-chip-value to-bot-num]} instr]
       (assign-chip state :bot to-bot-num give-chip-value)))
   state
   input))

(defn process-move-chip
  "Process a single move chip instruction."
  [state instr & {:keys [debug] :or {debug false}}]
  (when debug (println ">>>" instr))
  (let [{:keys [bot-num gives-low-to low-dest gives-high-to high-dest]} instr
        bot-chips (into [] (get-in state [:bot bot-num]))
        low-chip (apply min bot-chips)
        high-chip (apply max bot-chips)]
    (-> state
        (update :actions #(conj % {:action :compare :a low-chip :b high-chip :bot-num bot-num}))
        (assign-chip gives-low-to low-dest low-chip)
        (drop-chip :bot bot-num low-chip)
        (assign-chip gives-high-to high-dest high-chip)
        (drop-chip :bot bot-num high-chip))))


(defn process-move-chips
  "Update state for move chip instructions, keeping track of the compare actions we take."
  [state instrs & {:keys [debug] :or {debug false}}]
  (loop [state state
         instrs instrs]
    (let [;; Group instructions by bot chip cound
          instrs-by-chip-count (->> instrs (group-by #(count (get-chips state :bot (get % :bot-num)))))
          ;; We are only interested in instructions for two-chip bots
          valid-instrs (get instrs-by-chip-count 2)
          _ (when debug (println "\t" "valid instructions" valid-instrs))
          ;; Gather remaining instructions to continue
          invalid-instrs (apply concat (vals (dissoc instrs-by-chip-count 2)))
          _ (when debug (println "\t" (count invalid-instrs) "instructions remaining"))
          state (reduce
                 (fn [acc input]
                   (process-move-chip acc input :debug debug))
                 state
                 valid-instrs)]
      (if (= 0 (count valid-instrs))
        state
        (recur state invalid-instrs)))))

(defn process
  "Process assign chips instructions then move chips instructions."
  [input]
  (-> {:bot {} :output {} :actions []}
      (process-assign-chips (filter :give-chip-value input))
      (process-move-chips (filter :gives-low-to input))))

(defn part01
  [input]
  (reduce
   (fn [_ {:keys [a b bot-num]}]
     (when
      (or (= [a b] [17 61])
          (= [b a] [17 61]))
       (reduced bot-num)))
   nil
   (->> (process input) :actions)))

(deftest test-part01
  (is (= 161 (part01 (parse-input puzzle-input)))))

(defn part02
  [input]
  (let [state (process input)]
    (->> [0 1 2]
         (map #(get-chips state :output %))
         (map first)
         (apply *))))

(deftest test-part02
  (is (= 133163 (part02 (parse-input puzzle-input)))))

(run-tests)
