(require '[clojure.string :as s])

;; md5 function from https://gist.github.com/jizhang/4325757

(import java.security.MessageDigest)
(defn md5 [^String s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (format "%032x" (BigInteger. 1 raw))))

(def input (s/trim-newline (slurp "2015/day04/input")))

(defn mine
  [input difficulty]
  (let [prefix (s/join (take difficulty (repeat "0")))]
    (reduce
     (fn [_ ndx]
       (let [checksum (md5 (str input ndx))]
         (when (s/starts-with? checksum prefix)
           (reduced ndx))))
     nil
     (range))))

(mine "abcdef" 5) ;; => 609043
(mine "pqrstuv" 5) ;; => 1048970

;; Part 1

(mine input 5) ;; => 117946

;; Part 2

(mine input 6) ;; => 3938038
