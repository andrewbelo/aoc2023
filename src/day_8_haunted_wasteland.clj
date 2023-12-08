(ns day-8-haunted-wasteland
  (:require
    [clojure.math.numeric-tower :as math]
    [clojure.string :as str]))


(defn navigation
  [nav-line]
  (let [[k v] (str/split nav-line #" = ")
        [left right] (str/split v #", ")]
    [k {:left (str/replace left #"\(" "")
        :right (str/replace right #"\)" "")}]))


(defn network
  [text]
  (let [[inst-line _ & navs] (str/split-lines text)]
    {:instructions (map #(case % \R :right :left) (seq inst-line))
     :navigation (into {} (map navigation navs))}))


(defn move
  [[network node step] next-move]
  (let [next-node (next-move ((:navigation network) node "ZZZ"))
        result    [network next-node (inc step)]]
    (if (= \Z (last next-node)) (reduced result) result)))


(defn find-path-1
  [network start]
  (reduce move [network start 0] (cycle (:instructions network))))


(defn final-calc
  [part text]
  (let [network     (network text)
        navigation  (:navigation network)
        start   (case part
                  1 ["AAA"]
                  (into [] (filter #(= \A (last %)) (keys navigation))))]
    (reduce math/lcm (for [s  start]
                       (peek (find-path-1 network s))))))


(comment
  ;; Testing
  (def result  #(final-calc 1 %))
  (def result-2  #(final-calc 2 %))
  (def example
    "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)")

  (network example)
  (result example)
  (result-2 example)

  (def day 8)
  (def test-data (slurp (str "resources/day_" day ".txt")))

  (result test-data) ; 20777
  (result-2 test-data) ; 13289612809129
  (time (result test-data)) ; (out) "Elapsed time: 11.293746 msecs"
  (time (result-2 test-data)) ; (out) "Elapsed time: 37.683229 msecs"
  )
