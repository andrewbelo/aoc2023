(ns y-2015.d1
  (:require
    [clojure.string :as str]))


(comment 
  ;; Rich comment
  (def day 1)
  (def test-file (str "resources/2015/" day))
  (last (slurp test-file))

  (reduce (fn [[a i] b] (if (= b \() 
                               [(inc a) (inc i)] 
                               [(dec a) (inc i)])) 
            [0 0] (str/trim (slurp test-file)))

  (reduce (fn [[a i] b] 
            (if (= a -1) (reduced [a i])
              (if (= b \() 
                     [(inc a) (inc i)] 
                     [(dec a) (inc i)]))) 
            [0 0] (str/trim (slurp test-file))) ; [-1 1797]
  )
