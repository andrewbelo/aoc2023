(ns y-2023.day-18-digginings
  (:require
    [clojure.set :as set]
    [clojure.string :as str]))


(defn final-calc
  [part text]
  (let [smth (case part
               1 ())]
    (->> text)))


(comment
  ;; Testing
  (def example
    "

    ")

  (final-calc 1 example)
  (final-calc 2 example)

  (def day 5)
  (def test-data (slurp (str "resources/day_" day ".txt")))

  (final-calc 1 test-data)
  (final-calc 2 test-data)
  (time (final-calc 1 test-data))
  (time (final-calc 2 test-data))

  )
