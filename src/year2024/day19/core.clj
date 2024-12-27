(ns year2024.day19.core
   (:require [clojure.string :as s]
             [clojure.walk :as w]
             [clojure.pprint :refer [pprint]]
             [clojure.test :refer [deftest is run-tests]]))

 (def test-file "resources/year2024/day19/test-input")
 (def puzzle-file "resources/year2024/day19/input")

 (defn parse-input
   [file]
   (let [[available _ desired] (->> (-> (slurp file) s/split-lines)
                                    (partition-by #(= % "")))]
     {:available (s/split (first available) #", ")
      :desired (vec desired)}))

;; The issues here are:

;; a) we don't need to realise the entire tree as we can short-circuit when
;; the first successful permutation is found

 (defn towel-tree-old
   "Return lazy tree of towel combinations based on available prefixes xs."
   [xs t]
   (when-let [prefixes (filter #(s/starts-with? t %) xs)]
     (map #(lazy-seq (cons % (towel-tree xs (subs t (count %)))))) prefixes))

(do
;;;

  (defn towel-tree
    "Return lazily generated tree of matching stripes on towel based on
     available prefixes xs."
    ([xs t]
     {:root t
      :depth 0
      :children (towel-tree xs t 1)})
    ([xs t depth]
     (let [prefixes (filter #(s/starts-with? t %) xs)]
       (when (not (empty? prefixes))
         (map
          (fn [prefix]
            (hash-map :prefix prefix
                      :depth depth
                      :children (map #(towel-tree xs (subs t (count %)) (inc depth)) prefixes)))
          prefixes)))))

  (let [{:keys [available desired]} (parse-input test-file)]
    (->> (towel-tree available (first desired))
         (tree-seq
          ;; Is a branch?
          #(do
             #_(println "=> branch?" % (map? %))
             (map? %))
          #(do
             #_(println "=> children" % (get % :children))
             (get % :children)))
         first))

  ;;;
  )

 (defn tree-to-paths
   "Lazily generate seq of paths through a tree."
   [tree]
   (mapcat
    (fn [xs]
      (let [k (first xs)
            v (rest xs)]
        (if (empty? v)
          [[k]]
          (map #(cons k %) (tree-to-paths v)))))
    tree))

 (def tree-to-paths-xf
   (mapcat
    (fn [xs]
      (let [k (first xs)
            v (rest xs)]
        (if (empty? v)
          [[k]]
          (map #(cons k %) (tree-to-paths v)))))))

 (comment

   (do
    ;;;

     (def available-big (->> (parse-input puzzle-file) :available))
     (def desired-big "wrwubwgbbwgrrrugwwruburrrbwgrrruwwbggrbugbbwgbrb" #_"wrwubwgbbwgrrrugwwruburrrbwgrrruwwbggrbugbbwgbrb")

     (defn some-xf
       "A reducing function with the same semantics as some, but calls the
       reduced function when the predicate is true to short circuit the
       transduction operation."
       [pred]
       (fn [rf]
         (fn
           ([] (rf))
           ([result] (rf result))
           ([result input]
            (let [v (pred input)]
              (if v (reduced (rf result v)) result))))))

     (def tree-to-paths-xf
       (fn [rf]
         (fn
           ([] (rf))
           ([result] (rf result))
           ([result input]
            #_(println "=> tree-to-path-xf" input)
            (println "tree-seq returned" (->> input
                                              (tree-seq
                                               #(not (nil? (next %)))
                                               #(next %))))))))

     (let [available ["abab" "ab"]
           desired "abababab"
           tt (towel-tree available desired)]
       tt
       #_(transduce
          (comp
           #_tree-to-paths-xf
           #_(map println))
          conj
          []
          (towel-tree available desired)))

     #_(tree-seq
        #(do
           (let [has-branch (not (nil? (next %)))]
             (println "=> branch?" "first" (first %) "next" (next %) has-branch)
             #_(println "branch?" % has-branch)
             has-branch))
        #(do
           (let [children (next %)]
             (println "=> children" (next %))
             #_(println "children" %)
             children))
        (first (towel-tree available desired)))

  ;;=> (("ababab" ("ab") ("a" ("b")))
  ;;    ("abab" ("abab") ("ab" ("ab") ("a" ("b"))) ("a" ("b" ("ab") ("a" ("b")))))
  ;;    ("ab"
  ;;     ("ababab")
  ;;     ("abab" ("ab") ("a" ("b")))
  ;;     ("ab" ("abab") ("ab" ("ab") ("a" ("b"))) ("a" ("b" ("ab") ("a" ("b")))))
  ;;     ("a" ("b" ("abab") ("ab" ("ab") ("a" ("b"))) ("a" ("b" ("ab") ("a" ("b")))))))
  ;;    ("a"
  ;;     ("b"
  ;;      ("ababab")
  ;;      ("abab" ("ab") ("a" ("b")))
  ;;      ("ab" ("abab") ("ab" ("ab") ("a" ("b"))) ("a" ("b" ("ab") ("a" ("b")))))
  ;;      ("a" ("b" ("abab") ("ab" ("ab") ("a" ("b"))) ("a" ("b" ("ab") ("a" ("b")))))))))

  ;; Use thread-last
   #_(let [available ["ababab" "abab" "ab" "a" "b"]
           desired "abababab"]
       (->> (towel-tree available desired)
            tree-to-paths
            (map s/join)
            (filter #(= % desired))))

  ;; Use transduce
   (let [available (->> available-big (sort #(> (count %1) (count %2)))) #_["1234" "12" "34" "56" "78"]
         desired desired-big #_"12345678"]
     (transduce
      (comp
       (map tree-to-paths-xf)
       (map s/join)
       (some-xf #(= % desired)))
      conj
      []
      (towel-tree available desired)))

    ;; Let's try again. I think the idea of creating the entire tree lazily
    ;; is fine but I suspect the tree-to-paths-xf transducer is not
    ;; terminating as soon as it can.

    ;;;
   )
  ;;;
 )

(defn part01
  [file]
  (let [{:keys [available desired]} (parse-input file)
        towel-paths (map vector desired (map #(towel-tree available %) desired))]
    (->> towel-paths
         (map (fn [[towel paths]]
                [towel
                 (->> paths
                      tree-to-paths
                      (map s/join)
                      (filter #(= towel %)))]))
         (filter #(pos? (count (second %))))
         count)))

(comment
  (do
    ;;;

    (def tree-to-path-xf
      (mapcat
       (fn [xs]
         (let [k (first xs)
               v (rest xs)]
           (if (empty? v)
             [[k]]
             (map #(cons k %) (tree-to-paths v)))))))

    (let [desired "abab"]
      (->> desired
           (towel-tree ["ab" "a" "b"])
           #_tree-to-paths))



    (let [desired "abab"]
      (transduce
       (comp
        #_(map #(when (= desired (s/join %)) (reduced %) (println (s/join %))))
        (filter (fn [x] (println "=>" (s/join x) "<=") true))
        tree-to-path-xf)
       conj
       (towel-tree ["ab" "a" "b"] desired)))

    ;; towel -> tree of paths -> filter out incomplete paths ->

    #_(def foo
        (let [input (parse-input puzzle-file)
              available (:available input)
              desired (take 1 (:desired input))]
          (towel-tree available (first desired))))

    #_(let [{:keys [available desired]} (parse-input test-file)
            desired (take 2 desired)]
        (transduce
         (comp
          (map #(vector % (towel-tree available %))))
         conj
         []
         desired))

    ;; How about applying clojure.walk/walk o the return value for towel-tree
    ;; and short circuit via reduce when we have made a match with the entire
    ;; string?

    #_(let [desired "abab"]
        (w/postwalk
         (fn [x]
           (cond
          ; Strings map to themselves
             (string? x) x
          ; Single element lists become strings
             (= 1 (count x)) (first x)
          ; Join lists of strings together
             (= (count x) (count (keep #(string? %) x))) (s/join x)
             :else
             (do (pprint x) x)))
         (towel-tree ["ab" "a" "b"] desired)))


    ;;;
    )
;;;
  )

(deftest test-part01
  (is (= 6 (part01 test-file)))
  #_(is (= nil (part01 puzzle-file))))

(run-tests)
