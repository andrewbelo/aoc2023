(ns y-2015.d2
  (:require
    [clojure.string :as str]))


(comment 
  ;; Rich comment
  (def day 2)
  (def test-file (str "resources/2015/" day))

  (reduce (fn [r line]
             (let [[l w h] (map read-string (str/split line #"x"))
                   sides [(* 2 l w) (* 2 w h) (* 2 l h)]
                   ]
               (apply + r (/ (apply min sides) 2) sides)))
          0
          (str/split-lines (slurp test-file))) ; 1606483

  (reduce (fn [r line]
             (let [[l w h] (sort (map read-string (str/split line #"x")))]
               (+ r (* l w h) (+ l l w w))
               ))
          0
          (str/split-lines (slurp test-file))) ; 3842356

  (sort [1 2 3])
  )
