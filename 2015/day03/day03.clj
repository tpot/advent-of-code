(require '[clojure.set :as [set]])

(defn new-pos
  [input [x y]]
  (cond
    (= input \<) [(dec x) y]
    (= input \>) [(inc x) y]
    (= input \^) [x (inc y)]
    (= input \v) [x (dec y)]))

;; Part 1

(def input (slurp "2015/day03/input"))

(def visit
  (partial
   reduce
   (fn [{:keys [pos visited]} input]
     (let [new-pos (new-pos input pos)]
       {:pos new-pos
        :visited (conj visited new-pos)}))
   {:pos [0 0]
    :visited #{[0 0]}}))

(count (get (visit ">") :visited)) ;; => 2
(count (get (visit "^>v<") :visited)) ;; => 4
(count (get (visit "^v^v^v^v^v") :visited)) ;; => 2

(count (get (visit input) :visited)) ;; => 2572

;; Part 2

(defn robo-visit
  [input]
  (reduce
   (fn [result [santa-input robot-input]]
     (let [santa-new-pos (new-pos santa-input (get-in result [:santa :pos]))
           robot-new-pos (new-pos robot-input (get-in result [:robot :pos]))]
       {:santa {:pos santa-new-pos
                :visited (conj (get-in result [:santa :visited]) santa-new-pos)}
        :robot {:pos robot-new-pos
                :visited (conj (get-in result [:robot :visited]) robot-new-pos)}}))
   {:santa {:pos [0 0]
            :visited #{[0 0]}}
    :robot {:pos [0 0]
            :visited #{[0 0]}}}
   (partition 2 input)))

(defn unique-visits
  [state]
  (->> state vals (map :visited) (apply set/union)))

(count (unique-visits (robo-visit "^v"))) ;; => 3
(count (unique-visits (robo-visit "^>v<"))) ;; => 3
(count (unique-visits (robo-visit "^v^v^v^v^v"))) ;; => 11

(count (unique-visits (robo-visit input))) ;; => 2631
