(def start-floor 0)

;; Part 1

(let [directions (slurp "2015/day01/input")]
  (reduce
   (fn [result input]
     (cond
       (= input \()
       (inc result)
       (= input \))
       (dec result)))
   start-floor
   directions)) ; => 232

;; Part 2

(let [directions (slurp "2015/day01/input")]
  (reduce
   (fn [result [ndx input]]
     (let [floor (cond
                   (= input \()
                   (inc result)
                   (= input \))
                   (dec result))]
       (if (= floor -1) (reduced (inc ndx)) floor)))
   start-floor
   (map-indexed vector directions))) ; => 1783
