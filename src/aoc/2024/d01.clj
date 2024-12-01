(ns aoc.2024.d01
  (:require
    [clojure.string :as str]))


(def day 1)
(def test-txt (slurp (str "resources/2024/" day)))
(def example-txt (slurp (str "resources/2024/" day "e")))


(defn txt->cols
  [txt]
  (reduce
    (fn [[lhs rhs] line]
      (let [[l r] (map parse-long (str/split line #" +"))]
        [(conj lhs l) (conj rhs r)]))
    [[] []]
    (str/split-lines txt)))


(defn sol-1
  [input]
  (let [[lhs rhs] (txt->cols input)]
    (apply + (map (fn [a b] (Math/abs (- a b))) (sort lhs) (sort rhs)))))


(defn sol-2
  [input]
  (let [[lhs rhs] (txt->cols input), rhs-freq (frequencies rhs)]
    (apply + (map #(* % (rhs-freq % 0)) lhs))))


(comment
  (time (sol-1 test-txt)) ; 2742123
  ; (out) "Elapsed time: 46.354987 msecs"
  (time (sol-2 test-txt)) ; 21328497
  ; (out) "Elapsed time: 10.722665 msecs"
  )
