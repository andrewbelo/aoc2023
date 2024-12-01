(ns y-2015.d10
  (:require
    [clojure.data.priority-map :refer [priority-map]]
    [clojure.set :as set]
    [clojure.string :as str]))


(comment 
  ;; Rich comment
  (def day 10)
  (def line "1113122113")

  (defn res [line times]
    (count  (reduce (fn [line i]
                      (->>
                        line
                        (partition-by identity)
                        (mapcat (juxt count first ) )
                        (apply str))) 
                    line
                    (range times))))

  (time (res line 40))
  (time (res line 50))
  ; (out) "Elapsed time: 712.880917 msecs"
  ; 360154
  ; eval (root-form): (time (res line 50))
  ; (out) "Elapsed time: 9969.506103 msecs"
  ; 5103798


  (take 10 (sort-by second  (search nodes)))
  (take 10 (sort-by second > (search nodes)))
  (search lines-ex)
  )
