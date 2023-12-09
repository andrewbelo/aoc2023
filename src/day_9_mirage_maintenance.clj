(ns day-9-mirage-maintenance
  (:require
    [clojure.string :as str]))


(defn diffs
  [numbers]
  (loop [d    numbers
         edge [[] []]]
    (let [new-edge  (mapv conj edge [(first d) (last d)])
          new-d     (map #(apply - (reverse %)) (partition 2 1 d))]
      (if (empty? (remove zero? d))
        new-edge
        (recur new-d new-edge)))))


(defn extrapolate-edges
  [[l-edge r-edge]]
  [(reduce #(- %2 %1) (reverse l-edge)) (apply + r-edge)])


(defn predict
  [line]
  (->> (str/split line #" ")
       (map read-string)
       (diffs)
       (extrapolate-edges)))


(defn final-calc
  [part text]
  (let [result (case part 1 second first)]
    (->> text
         (str/split-lines)
         (map predict)
         (map result)
         (apply +))))


(comment
  ;; Testing
  (def result  #(final-calc 1 %))
  (def result-2  #(final-calc 2 %))
  (def example
"0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45")

  (map #(- (apply - %)) (partition 2 1 [3 3 3 3 3 3 3]))
  (diffs [0 3 6 9 12 15])
  (predict "10 13 16 21 30 45")
  (result example)
  (result-2 example)

  (def day 9)
  (def test-data (slurp (str "resources/day_" day ".txt")))

  (result test-data) ; 1647269739
  (result-2 test-data) ; 864
  (time (result test-data)) ; (out) "Elapsed time: 34.499368 msecs"
  (time (result-2 test-data)) ; (out) "Elapsed time: 53.027693 msecs"

  )
