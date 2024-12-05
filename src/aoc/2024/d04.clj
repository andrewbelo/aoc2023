(ns aoc.2024.d04
  (:require
    [clojure.string :as str]))


(def day 4)
(def test-txt (slurp (str "resources/2024/" day)))
(def example-txt (slurp (str "resources/2024/" day "e")))
(def dirs [[0 1] [1 1] [1 0] [1 -1]])
(def edges [[-1 -1] [1 1] [-1 1] [1 -1]])


(defn txt->
  [txt]
  (mapv #(str/split % #"") (str/split-lines txt)))


(defn move-vec-by
  [a dir i]
  (mapv #(+ %1 (* i %2)) a dir))


(defn xmas-starts
  [txt row col]
  (if (nil? (#{"X" "S"} (get-in txt [row col] \space)))
    '()
    (concat
      (for [dir dirs
            :let [coords (mapv #(move-vec-by [row col] dir %) (range 4))
                  line (str/join (mapv #(get-in txt % \space) coords))]
            :when (#{"XMAS" "SAMX"} line)]
        line))))


(defn x-mas-start?
  [txt row col]
  (when (= (get-in txt [row col]) "A")
    (let [[a b c d] (for [edge edges
                          :let [pos' (mapv + [row col] edge)]]
                      (get-in txt pos' \space))
          diagonals (map str/join [[a b] [c d]])]
      (every? some? (map #{"MS" "SM"} diagonals)))))


(defn sol-1
  [input]
  (let [txt (txt-> input)
        starts (for [r (range (count txt))
                     c (range (count (first txt)))]
                 (xmas-starts txt r c))]
    (count (apply concat starts))))


(defn sol-2
  [input]
  (let [txt     (txt-> input)
        starts  (for [r (range (count txt))
                      c (range (count (first txt)))]
                  (x-mas-start? txt r c))]
    (count (filter true? starts))))


(comment
  (def ex (txt-> example-txt))
  (xmas-starts ex 0 1)

  ; (out) "Elapsed time: 333.561394 msecs"
  ; (out) "Elapsed time: 138.758496 msecs"
  (time (sol-1 test-txt)) ; 2583

  ; (out) "Elapsed time: 34.816937 msecs"
  (time (sol-2 test-txt)) ; 1978
  )
