(ns year2024.day15.core
  (:require [clojure.string :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.test :refer [deftest is run-tests]]))

(def test-file "resources/year2024/day15/test-input")
(def puzzle-file "resources/year2024/day15/input")

;; Objects
(def EMPTY \.)
(def BOX   \O)
(def WALL  \#)
(def ROBOT \@)

(def BIGBOX-L \[)
(def BIGBOX-R \])

;; Movement commands
(def MOVE-UP    \^)
(def MOVE-DOWN  \v)
(def MOVE-LEFT  \<)
(def MOVE-RIGHT \>)

;; Directions
(defn north [[row col]] [(dec row) col])
(defn south [[row col]] [(inc row) col])
(defn east  [[row col]] [row       (inc col)])
(defn west  [[row col]] [row       (dec col)])

(defn dir->sym
  [dir]
  (get (into {} [[north :north] [south :south] [east :east] [west :west]]) dir))

(defn parse-input
  [file]
  (let [[board _ moves] (->> (-> (slurp file) s/split-lines)
                             (partition-by #(= "" %)))]
    {:board (mapv vec board)
     :moves (vec (s/join "" moves))}))

(defn locations
  "Find all locations of an object "
  [board ch]
  (apply
   concat
   (for [row (range (count board))]
     (map
      #(vector row %)
      (keep-indexed (fn [i x] (when (= x ch) i)) (get board row))))))

(defn move-object
  "Move an object at location `start-pos` to the location calculated from
   function `dir-fn`."
  [board start-pos dir-fn & {:as opts}]
  (when (:debug opts)
    (println "\t\t=> Move" (get-in board start-pos) "from" start-pos "to" (dir-fn start-pos)))
  (when (get-in board (dir-fn start-pos))
    (-> board
        (update-in (dir-fn start-pos) (constantly (get-in board start-pos)))
        (update-in start-pos (constantly EMPTY)))))

(defn dir-seq
  "Return lazy sequence of coordinates starting at position and heading in direction."
  [dir pos]
  (let [new-pos (dir pos)]
    (lazy-seq (cons pos (dir-seq dir new-pos)))))

(def move-dir
  {MOVE-UP    north
   MOVE-DOWN  south
   MOVE-RIGHT east
   MOVE-LEFT  west})

(defn distances-to
  "Given a seq of objects (a line of sight) return the distances to
   visible objects."
  [xs]
  (->> xs
       (take-while (fn [[_ x]] (not (nil? x))))
       (keep-indexed (fn [ndx x] (vector ndx (first x) (second x))))))

(defn distance-to
  "Given a seq of objects (a line of sight) return the distance to
   visible object of a type"
  [xs ch]
  (->> (distances-to xs)
       (filter #(= ch (nth % 2)))
       first))

(defn push
  "Push objects in a direction starting at a position."
  [board line-of-sight dir dist]
  (reduce
   (fn [result input] (move-object result input dir))
   board
   (reverse
    (sequence
     (comp (map first) (take dist))
     line-of-sight))))

(defn perform-move
  "Return new board after applying a single move."
  [{:keys [robot-pos board] :as state} input]
  (let [dir (get move-dir input)
        ;; Seq of [pos x] along the direction of movement
        line-of-sight (map #(vector % (get-in board %)) (dir-seq dir robot-pos))
        ;; Distance to the closest empty position closest wall
        [empty-dist _empty-pos _] (distance-to line-of-sight EMPTY)
        [wall-dist  _wall-pos  _] (distance-to line-of-sight WALL)]
    (if (and empty-dist (< empty-dist wall-dist))
      {:robot-pos (dir robot-pos)
       :board (push board line-of-sight dir empty-dist)}
      state)))

(defn perform-moves
  "Return new board after applying a sequence of moves."
  [board moves]
  (->> (reduce
        (fn [state move] (perform-move state move))
        {:board board
         :robot-pos (first (locations board ROBOT))}
        moves)
       :board))

(defn gps
  [[x y]]
  (+ x (* 100 y)))

;; Part 1

(defn part01
  "Count the number of unique points the guard passed through."
  [file]
  (let [{:keys [board moves]} (parse-input file)
        new-board (perform-moves board moves)
        boxes (locations new-board BOX)]
    (->> boxes
         (map reverse)
         (map gps)
         (apply +))))

(deftest test-part01
  (is (= 10092 (part01 test-file)))
  (is (= 1406392 (part01 puzzle-file))))

;; Part 2

;; Unfortunately requires a rewrite of the algorithm used. Use a recursive
;; function to push a block by first pushing on the block ahead and
;; terminating if the spot to be pushed into is empty or a wall.

(defn ch->big-ch
  [ch]
  (condp = ch
    BOX [BIGBOX-L BIGBOX-R]
    ROBOT [ROBOT EMPTY]
    [ch ch]))

(defn board->big-board
  [board]
  (->> board
       (mapv #(vec (mapcat ch->big-ch %)))))

(defn push2
  "Push in a direction from a position."
  [board dir pos & {:as opts}]
  (let [this (get-in board pos)
        ahead (get-in board (dir pos))]
    (when (:debug opts)
      (println "\t=> At" pos "push" this "from the" (dir->sym dir) "onto" ahead))
    (cond
      ; Terminating condition - moving into wall passed up by a nil board
      (nil? board)
      nil
      ; Terminating condition - push into empty space
      (= ahead EMPTY)
      (move-object board pos dir opts)
      ; Terminating condition - can't move into wall
      (= ahead WALL)
      nil
      ; Push box from the east or west
      (or (= dir east) (= dir west))
      (-> board
          (push2 dir (dir pos) opts)
          (move-object pos dir opts))
      ; Push box from the north or south
      (or (= dir north) (= dir south))
      (let [side-dir (if (= ahead BIGBOX-L) east west)
            side-pos (side-dir pos)]
        (-> board
            (push2 dir (dir side-pos) opts)
            (push2 dir (dir pos) opts)
            (move-object pos dir opts))))))

(defn perform-moves2
  [board moves & {:as opts}]
  (->> (reduce
        (fn [board move]
          (when (:debug opts)
            (println "=> Perform move" move)
            (pprint (map s/join board)))
          ; If move would fail then don't update board
          (or (push2 board (move-dir move) (first (locations board ROBOT)) opts)
              board))
        board
        moves)))

 (defn part02
  [file & {:as opts}]
  (let [{:keys [board moves]} (parse-input file)
        new-board (perform-moves2 (board->big-board board) moves opts)
        _ (and (:debug opts) (->> new-board (map s/join) pprint))
        boxes (locations new-board BIGBOX-L)]
    (->> boxes
         (map reverse)
         (map gps)
         (apply +))))

(deftest test-part02
  (is (= 9021 (part02 test-file)))
  (is (= 1429013 (part02 puzzle-file))))

(run-tests)
