(ns year2023.day02.core
  (:require [clojure.string :as s]))

(def puzzle-input (-> "resources/year2023/day02/input" slurp s/split-lines))

;; Part 1

(def MAX-RED 12)
(def MAX-GREEN 13)
(def MAX-BLUE 14)

(defn parse-game-id [s]
  (let [[_ id rest] (re-matches #"Game (\d+): (.*)" s)]
    {:game-id (parse-long id) :rest rest}))

(defn parse-round [s]
  (let [[_ count color] (re-matches #"(\d+) (.+)" s)]
    {:count (parse-long count) :color (keyword color)}))

(defn parse-rounds [s]
  (->> (s/split s #", ")
       (map parse-round)))

(defn parse-game [line]
  (let [{:keys [game-id rest]} (parse-game-id line)
        rounds (map parse-rounds (s/split rest #"; "))]
    {:game-id game-id :rounds rounds}))

(defn possible-round?
  "Check if round violates max cubes constraint."
  [round]
  (reduce
   (fn [_ input]
     (cond
       (and (= (:color input) :blue) (> (:count input) MAX-BLUE))
       (reduced false)
       (and (= (:color input) :red) (> (:count input) MAX-RED))
       (reduced false)
       (and (= (:color input) :green) (> (:count input) MAX-GREEN))
       (reduced false)
       :else true))
   true
   round))

(defn possible-game?
  "Check if rounds of a game violate max cubes constraints."
  [game]
  (reduce
   (fn [_ input]
     (if-not (possible-round? input)
       (reduced false)
       true))
   true
   (:rounds game)))

(comment

  (->> (map parse-game puzzle-input)
       (map #(hash-map :game-id (:game-id %) :possible? (possible-game? %)))
       (filter :possible?)
       (map :game-id)
       (reduce + 0)) ;; => 2913  
  )

;; Part 2: What is the fewest number of cubes to make each game possible?

(defn update-max-colors
  [max-colors round]
  (reduce
   (fn [result input]
     (merge
      result
      (when (= (:color input) :red)   {:red   (max (:red result)   (:count input))})
      (when (= (:color input) :blue)  {:blue  (max (:blue result)  (:count input))})
      (when (= (:color input) :green) {:green (max (:green result) (:count input))})))
   max-colors
   round))

(defn get-max-colors
  [game]
  (reduce
   (fn [result input]
     (update-max-colors result input))
   (zipmap [:red :blue :green] (repeat 0))
   (:rounds game)))

(comment

  (->> (map parse-game puzzle-input)
       (map get-max-colors)
       (map #(apply * (vals %)))
       (reduce + 0)) ;; => 55593
  )