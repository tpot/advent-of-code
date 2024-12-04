(def input
  (map
   (comp (partial map parse-long) (partial drop 1))
   (re-seq #"(\d+)x(\d+)x(\d+)" (slurp "2015/day02/input"))))

;; Part 1

(defn area
  [length width height]
  (+ (* 2 length width) (* 2 width height) (* 2 height length)))

(defn slack
  [length width height]
  (min (* length width) (* width height) (* height length)))

(->> input
     (map (fn [[length width height]]
            (+ (area length width height)
               (slack length width height))))
     (apply +)) ;; => 1586300

;; Part 2

(defn perimeter
  [x y]
  (* 2 (+ x y)))

(defn volume
  [length width height]
  (* length width height))

(->> input
     (map (fn [[length width height]]
            (+ (min (perimeter length width)
                    (perimeter length height)
                    (perimeter width height))
               (volume length width height))))
     (apply +)) ;; => 3737498
