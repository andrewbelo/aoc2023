(ns day-2-2015
  (:require
    [clojure.string :as str]))


(defn calc-line
  [line]
  (println line)
  (let [[l w h] (map read-string (str/split line #"x"))
        sides (map (fn [[a b]] (* a b)) [[l w] [w h] [h l]])]
    [(+ (apply + (map #(* 2 %) sides)) (apply min sides))
     (+  (* l w h) (* 2 (apply + (take 2 (sort [l w h])))))]))


(calc-line "1x1x10")
(calc-line "2x3x4")


(defn calc
  [part text]
  (let [result (case part 1 first second)]
    (->> text
         (str/split-lines)
         (map calc-line)
         (map result)
         (apply +))))


(def day 2)
(def test-data (slurp (str "resources/2015_day_" day ".txt")))
(calc 1 test-data)
(calc 2 test-data)
