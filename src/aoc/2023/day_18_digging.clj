(ns day-18-digging
  (:require
    [clojure.pprint :refer [pprint]]
    [clojure.set :as set]
    [clojure.string :as str]))


(def delta {:U [-1 0], :R [0 1], :D [1 0], :L [0 -1]})


(defn- parse-inst
  [line]
  (let [directions  {"R" [0 1] "L" [0 -1] "U" [-1 0] "D" [1 0]}
        [_ dir len color] (re-matches #"^([URDL]) (\d+) \(#([0-9a-f]{6})\)"
                                      line)]
    [(delta (keyword dir)) (parse-long len) color]))


(defn- shoelace
  [[[y1 x1] [y2 x2]]]
  (- (* x1 y2) (* y1 x2)))


(defn- calculate
  [points]
  (+ (shoelace (list (last points) (first points)))
     (reduce + (map shoelace (partition 2 1 points)))))


(defn- solve
  [insts]
  (loop [[inst & insts] insts, poly (list [0 0]), p_cnt 0]
    (if (nil? inst)
      (inc (quot (+ (calculate (reverse poly)) p_cnt) 2))
      (let [[dir len] inst
            lp        (first poly)
            newpos    (mapv + lp (mapv * [len len] dir))]
        (recur insts (cons newpos poly) (+ p_cnt len))))))


(defn part-1
  "Day 18 Part 1"
  [input]
  (->> input
       (str/split-lines)
       (map parse-inst)
       solve))


(defn- fix-inst
  [[_ _ color]]
  (let [dirs {"0" :R, "1" :D, "2" :L, "3" :U}
        [_ val dir] (re-matches #"([0-9a-f]{5})([0-3])" color)]
    [(delta (dirs dir)) (Integer/parseInt val 16)]))


(defn part-2
  "Day 18 Part 2"
  [input]
  (->> input
       (str/split-lines)
       (map parse-inst)
       (map fix-inst)
       solve))


(comment
  ;; Rich comment

  (def day 18)
  
  (part-1 (slurp (str "resources/2023/" day )))
  (part-2 (slurp (str "resources/2023/" day )))
  )
