(ns day-17-second-try
  (:require
    [clojure.data.priority-map :refer [priority-map]]
    [clojure.pprint :refer [pprint]]
    [clojure.string :as str]))


(def turns
  {:east [:south :north]  :west [:south :north]
   :south [:west :east]   :north [:west :east]})


(def dir-meaning {:east [0 1], :west [0 -1], :south [1 0], :north [-1 0]})
(def EAST (dir-meaning :east))
(def WEST (dir-meaning :west))
(def SOUTH (dir-meaning :south))
(def NORTH (dir-meaning :north))


(defn txt->nodes
  [text]
  (mapv #(mapv read-string (str/split % #"")) (str/split-lines text)))


(def lookfor
  #{[[1 2] [0 1] 1]
    [[1 3] [0 1] 2]
    [[1 4] [0 1] 3]
    [[1 4] [0 1] 1]
    [[1 5] [-1 0] 1]
    [[0 5] [0 1] 1]})


(defn move
  [[pos dir in-a-row :as org] [curr-heat prev] city steps visited q]
  (for [d       [EAST WEST SOUTH NORTH]
        :when   (not= d (mapv #(* % -1) dir))
        :when   (if (= d dir) (< in-a-row 10) (>= in-a-row 4))
        :let    [[r c]  (mapv + pos dir)
                 state' [[r c] d (if (= d dir) (inc in-a-row) 1)]
                 [r' c'] (mapv + [r c] d)]
        :when   (and  (>= (dec (count city)) r' 0)
                      (>= (dec (count city)) r 0)
                      (>= (dec (count city)) c' 0)
                      (>= (dec (count city)) c 0))
        :let    [heat' (+ curr-heat ((city r) c))]
        :when   (or  (nil? (visited state'))
                     (< heat' (first (visited state'))))
        :when   (or  (nil? (q state'))
                     (< heat' (first (q state'))))]
    [state'  [heat' org]]))


(defn vector-min
  [a b]
  (if (< (compare a b) 0) a b))


(compare [2 [2 2]] [5 [2 2]])


(defn solve
  [city steps]
  (loop [curr-paths-ends  (priority-map [[0 0] SOUTH 1] [0 [0 0]]
                                        [[0 0] EAST 1] [0 [0 0]])
         visited  {}
         i        0]
    (let [[[[r c] _ _ :as state] heat]  (peek curr-paths-ends)]
      (cond

        (nil? state) nil

        (= r c (dec (count city)))  (first  heat)

        :else
        (recur (into (pop curr-paths-ends) (move state heat city steps visited curr-paths-ends))
               (assoc visited state heat)
               (inc i))))))


(comment 
  ;; Rich comment
  (def day 17)
  (def test-data (slurp (str "resources/day_" day ".txt")))
  (def exam-data (slurp (str "resources/day_" day "_ex.txt")))
  (def city (txt->nodes exam-data))
  (def vs  (solve (txt->nodes exam-data) [0 3]))
  (defn get-in-visited [visited [r c] ]
    (sort-by last 
    (for [d [EAST WEST SOUTH NORTH]
          in-a-row [0 1 2 3]
          :let [coord [[r c] d in-a-row]]
          :when (visited coord)]
      [d in-a-row (visited coord)]))
    )
  (get-in-visited vs [12 12])
  (sort-by last  (get-in-visited vs [1 4]))
  (move [[11 12] [1 0] 1] 100 city #{0 1 2 3})


  (+
   4 1 1 5 4 5 3 2 3 1;; 3 5 4 2 4 5 3 5 6 5 3 7 3 3 6 3 3 3
   )
  ;; 2 4 1 1 5 4 5 3
  ;;  4 5 6 11 15 20 23
 
  (def s (solve (txt->nodes exam-data) [0 3]))
  (first s)
  (get-in-visited (second s) [0 2])
  (get-in-visited (second s) [1 5])
  (get-in-visited (second s) [0 5])
  (get-in-visited (second s) [11 11])
  (solve (txt->nodes test-data) #{1 2 3}) ; [936 [[139 140] [1 0] 2]]
  (def p (priority-map :a 2 :b 1 :c 3 :d 5 :e 4 :f 3))
  (into p [[:a 1]])
  )
