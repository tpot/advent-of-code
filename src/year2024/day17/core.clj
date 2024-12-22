(ns year2024.day17.core
  (:require [clojure.string :as s]
            [clojure.math :refer [pow]]
            [clojure.pprint :refer [pprint]]
            [clojure.test :refer [deftest is run-tests]]))

(def test-file "resources/year2024/day17/test-input")
(def test-file2 "resources/year2024/day17/test-input2")
(def puzzle-file "resources/year2024/day17/input")

(defn parse-input
  [file]
  (let [[registers _ program] (->> (-> (slurp file) s/split-lines)
                                   (partition-by #(= % "")))]
    {:registers (->> registers
                     (map #(re-matches #"Register (.): (\d+)" %))
                     (map #(drop 1 %))
                     (map #(vector (keyword (first %)) (parse-long (second %))))
                     (into {}))
     :program (mapv
               parse-long
               (s/split
                (->> program
                     first
                     (re-matches #"Program: (.*)")
                     second) #","))}))

;;  Instructions/opcodes
(def opcode-list
  [[:ADV 0]   ; Division with register A
   [:BXL 1]   ; Bitwise XOR with register B
   [:BST 2]   ; Operand mod 8
   [:JNZ 3]   ; Jump if not zero
   [:BXC 4]   ; Bitwise XOR registers A and C
   [:OUT 5]   ; Output a value
   [:BDV 6]   ; Division with register B
   [:CDV 7]]) ; Division with register C

(def opcode-map
  (reduce
   (fn [m [ki vi]]
     (merge m (zipmap (map #(nth % ki)     opcode-list)
                      (map #(nth % vi nil) opcode-list))))
   {}
   [[0 1] [1 0]]))

;; Mapping of combo operands to literals or register values
(def combo-oper-map
  {0 0
   1 1
   2 2
   3 3
   4 :A
   5 :B
   6 :C
   7 :reserved})

(defn execute-instr
  "Return updated state after executing instruction and operand.'"
  [{:keys [registers ip output] :as _state} instr oper]
  (let [{:keys [A B C]} registers
        combo-oper (get registers (get combo-oper-map oper) oper)]
    {:registers (merge registers
                       ;; ADV, BDC and CDV
                       (when (= instr :ADV) {:A (int (/ A (pow 2 combo-oper)))})
                       (when (= instr :BDV) {:B (int (/ A (pow 2 combo-oper)))})
                       (when (= instr :CDV) {:C (int (/ A (pow 2 combo-oper)))})
                       ;; BXL, BST, BXC
                       (when (= instr :BXL) {:B (bit-xor B oper)})
                       (when (= instr :BST) {:B (mod combo-oper 8)})
                       (when (= instr :BXC) {:B (bit-xor B C)}))
     :ip (if (and (= instr :JNZ) (pos? A)) oper (+ ip 2))
     :output (if (= :OUT instr)
               (conj output (mod combo-oper 8))
               output)}))

(defn execute
  ([input] (execute input nil))
  ([input {:keys [debug]}]
   (let [{:keys [registers program]} input]
     (loop [state {:registers registers :ip 0 :output []}
            iter-count 100]
       ;; Terminating conditions
       (when debug (println "=>" state))
       (when (zero? iter-count) (throw (Exception. "Iteration count exceeded!")))
       ;; Decode instruction
       (let [ip (get state :ip)
             instr (get opcode-map (get program ip) nil)
             oper (get program (inc ip) nil)]
         (when debug (println "\t instr" instr "oper" oper))
         (if (or (nil? instr) (nil? oper))
           state
           (recur
            (execute-instr state instr oper)
            (dec iter-count))))))))

(defn part01
  [file & opts]
  (->> (execute (parse-input file) opts)
       :output
       (s/join ",")))

(deftest test-part01
  (is (= "4,6,3,5,6,3,5,2,1,0" (part01 test-file)))
  (is (= "3,1,5,3,7,4,2,7,5" (part01 puzzle-file))))

(defn execute-seq
  "Lazy sequence of program state after executing one instruction."
  ([program {:keys [_registers ip _output] :as state}]
   (let [instr (get opcode-map (get program ip) nil)
         oper (get program (inc ip) nil)]
     (if (or (nil? instr) (nil? oper))
       nil ; Halted
       (let [new-state (execute-instr state instr oper)]
         (lazy-seq (cons new-state (execute-seq program new-state))))))))

(defn program-matches-output?
  "Execute a program with inital register value of a and return true if it
   outputs its own program."
  [program a]
  (let [initial-state {:registers {:A a :B 0 :C 0} :ip 0 :output []}]
    (reduce
     (fn ([_ {:keys [output] :as _state}]
          (if (= output (take (count output) program))
              ;; Check if we match the entire program
            (when (= (count output) (count program)) (reduced true))
              ;; Bail out early if the output diverges from the program
            (reduced false))))
     nil
     (execute-seq program initial-state))))

(defn part02
  [file & {:keys [debug]}]
  (let [{:keys [_registers program]} (parse-input file)]
    (reduce
     (fn [_ a]
       (when (and debug (zero? (mod a 100000))) (println a))
       (when (program-matches-output? program a) (reduced a)))
     nil
     (range))))

(deftest test-part02
  (is (= 117440 (part02 test-file2)))
  ;; Still too slow to solve the problem ):
  #_(is (= nil (part02 puzzle-file))))

(run-tests)
