(ns aoc.2024.d04
  (:require
    [clojure.set :as set]
    [clojure.string :as str]))


(def day 4)
(def test-txt (slurp (str "resources/2024/" day)))
(def example-txt (slurp (str "resources/2024/" day "e")))


(defn txt->
  [txt]
  (mapv #(str/split % #"") (str/split-lines txt)))


(def dirs [[0 1] [1 1] [1 0] [1 -1]])


(defn xmas-start
  [txt row col]
  (concat
    (for [dir dirs
          :let [coords (mapv (fn [i] (mapv #(+ %1 (* i %2)) [row col] dir))
                             (range 4))
                ccs   (filter (fn [[a b]]
                                (and (<= 0 a (dec (count txt)))
                                     (<= 0 b (dec (count txt))))) coords)
                values (mapv (fn [[a b]] ((txt a) b)) ccs)
                conc (str/join values)]
          :when (#{"XMAS" "SAMX"} conc)]
      conc)))


(defn x-mas-start?
  [txt row col]
  (when (= ((txt row) col) "A")
    (let [edges (into [] (for [i (range 4)
                               :let [diffs [[-1 -1] [1 1] [-1 1] [1 -1]]
                                     [r' c'] (mapv + [row col] (diffs i))]
                               :when (and
                                       (<= 0 r' (dec (count txt)))
                                       (<= 0 c' (dec (count txt))))]
                           ((txt r') c')))]
      (and (= 4 (count edges))
           (#{"MS" "SM"} (str/join (subvec edges 0 2)))
           (#{"MS" "SM"} (str/join (subvec edges 2 4)))))))


([1 2 3] 2)


(defn sol-1
  [input]
  (let [txt (txt-> input)]
    (apply concat (for [r (range (count txt))
                        c (range (count (first txt)))
                        :let [starts (xmas-start txt r c)]
                        :when (seq starts)]
                    starts))))


(defn sol-2
  [input]
  (let [txt (txt-> input)]
    (for [r (range (count txt))
          c (range (count (first txt)))]
      (x-mas-start? txt r c))))


(count (filter (not= false)) (filter some? (sol-2 test-txt))) ; 2064
;; ()
;; 0
(count (filter true? (sol-2 example-txt)))
(def f (txt-> example-txt))
(x-mas-start? f 1 2)
(xmas-start f  0 4)
(xmas-start f  0 5)


(comment
  (time (sol-1 test-txt))

  (time (sol-2 test-txt))
; (nil
;  nil
;  nil
;  nil
;  false
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  false
;  nil
;  nil
;  nil
;  nil
;  nil
;  false
;  nil
;  nil
;  nil
;  nil
;  false
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  false
;  nil
;  false
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  false
;  nil
;  false
;  nil
;  nil
;  nil
;  nil
;  nil
;  false
;  false
;  nil
;  false
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  false
;  nil
;  nil
;  nil
;  false
;  nil
;  false
;  nil
;  nil
;  false
;  nil
;  nil
;  false
;  nil
;  nil
;  false
;  nil
;  nil
;  nil
;  nil
;  nil
;  false
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  false
;  nil
;  nil
;  nil
;  false
;  nil
;  nil
;  nil
;  false
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  false
;  nil
;  false
;  nil
;  nil
;  nil
;  false
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  "MS"
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  "SM"
;  nil
;  "MS"
;  nil
;  nil
;  "MS"
;  nil
;  "MS"
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  "MS"
;  "MS"
;  nil
;  "SM"
;  nil
;  nil
;  nil
;  "MS"
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  "SM"
;  nil
;  nil
;  nil
;  nil
;  "MS"
;  nil
;  nil
;  nil
;  "MS"
;  nil
;  nil
;  nil
;  nil
;  nil
;  "SM"
;  nil
;  "MS"
;  nil
;  nil
;  nil
;  "MS"
;  nil
;  nil
;  nil
;  "SM"
;  nil
;  "SM"
;  nil
;  nil
;  nil
;  "SM"
;  nil
;  "SM"
;  nil
;  "SM"
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  "SM"
;  nil
;  "SM"
;  nil
;  nil
;  "SM"
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  "MS"
;  "SM"
;  nil
;  "SM"
;  nil
;  "SM"
;  nil
;  "SM"
;  nil
;  nil
;  nil
;  nil
;  "SM"
;  nil
;  "SM"
;  nil
;  "SM"
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  "SM"
;  nil
;  "SM"
;  nil
;  nil
;  nil
;  nil
;  "SM"
;  nil
;  "SM"
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  "MS"
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  "MS"
;  nil
;  nil
;  nil
;  "SM"
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  "SM"
;  nil
;  nil
;  nil
;  "MS"
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  "SM"
;  nil
;  "SM"
;  nil
;  "SM"
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  "SM"
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  "SM"
;  nil
;  "SM"
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  "SM"
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  nil
;  ...)

  )
