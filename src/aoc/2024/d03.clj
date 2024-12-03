(ns aoc.2024.d03
  (:require
    [clojure.string :as str]))


(def day 3)
(def example-txt "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")
(def test-txt (slurp (str "resources/2024/" day)))


(defn sol-1
  [txt]
  (apply + (for [[_ lhs rhs] (re-seq #"mul\((\d+),(\d+)\)" txt)]
             (apply * (map parse-long [lhs rhs])))))


(defn sol-2
  [txt]
  (reduce
    (fn [[acc sw] [cmd lhs rhs]]
      (cond
        (str/starts-with? cmd "mul")
        (if sw
          [(+ acc (* (parse-long lhs) (parse-long rhs))) sw]
          [acc sw])

        (= cmd "don't()")
        [acc false]

        (= cmd "do()")
        [acc true]))
    [0 true]
    (re-seq #"mul\((\d+),(\d+)\)|do\(\)|don't\(\)" txt)))


(comment
  ; (out) "Elapsed time: 2.20926 msecs"
  (time (sol-1 test-txt))  ; 182619815
  ; (out) "Elapsed time: 1.401682 msecs"
  (time (sol-2 test-txt)) ; [80747545 true]
  )
