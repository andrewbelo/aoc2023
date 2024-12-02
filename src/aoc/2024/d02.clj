(ns aoc.2024.d02
  (:require
    [clojure.set :as set]
    [clojure.string :as str]))


(def day 2)
(def test-txt (slurp (str "resources/2024/" day)))
(def example-txt (slurp (str "resources/2024/" day "e")))


(defn txt->
  [txt]
  (mapv #(mapv parse-long %) (map #(str/split % #" ") (str/split-lines txt))))


(defn safe-1'?
  [report]
  (let [diffs (map #(apply - %) (partition 2 1 report))]
    (or (every? #(<= 1 % 3) diffs) (every? #(>= -1 % -3) diffs))))


(defn safe-1?
  [report]
  (every? true? (map (fn [[a b c]]
                       (and (or  (> a b c) (< a b c))
                            (<= 1 (Math/abs (- a b)) 3)
                            (<= 1 (Math/abs (- b c)) 3)))
                     (partition 3 1 report))))


(defn safe-2?
  [report]
  (or
    (safe-1? report)
    (some safe-1? (for [i (range (count report))]
                    (into (subvec report 0 i) (subvec report (inc i)))))))


(defn sol-1
  [input]
  (count (filter safe-1'? (txt-> input))))


(defn sol-2
  [input]
  (count  (filter safe-2? (txt-> input))))


(comment
  (time (sol-1 test-txt)) ; 279

  (time (sol-2 test-txt))  ; 1000

  )
