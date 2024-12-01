(ns y-2015.d4
  (:require
    [clj-commons.digest :refer [md5]]
    [clojure.set :as set]
    [clojure.string :as str]))


(md5 "jfdka")


(comment 
  ;; Rich comment
  (def day 4)
  (def test "yzbqklnj")

  (last (take-while #(not= "00000" (subs (md5 (str test %)) 0 5)) 
                    (range 10000000))) ; 282748
  (last (take-while #(not= "000000" (subs (md5 (str test %)) 0 6)) 
                    (range 10000000))) ; 9962623
  (md5 (str test 282748))
  (md5 (str test 282749))
  (md5 (str test 9962623))
  (md5 (str test 9962624))
  )
