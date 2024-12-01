(ns y-2016.d5
  (:require
    [clj-commons.digest :refer [md5]]
    [clojure.set :as set]
    [clojure.string :as str]))


(md5 "abc3231929")

(md5 "ugkcyxxp1294457")
(md5  "ugkcyxxp3337132")
(md5  "ugkcyxxp3548854")
(md5  "ugkcyxxp3919224")
(md5  "ugkcyxxp4473256")
(md5  "ugkcyxxp7112485")
(md5  "ugkcyxxp7573497")
(md5  "ugkcyxxp11291135")


(defn sol2
  [line]
  (reduce
    (fn [[visited looking] i]
      (let [h (md5 (str test i))
            d (nth h 5)
            v (nth h 6)]
        (if (and
              (= "00000" (subs h 0 5))
              (looking d))
          (if (= (count looking) 1)
            (reduced (assoc visited d [v i h]))
            [(assoc visited d [v i h]) (disj looking d)])
          [visited looking])))
    [{} #{\0 \1 \2 \3 \4 \5 \6 \7}]
    (range  100000000)))


(comment 
  ;; Rich comment
  (def day 4)
  (def test "ugkcyxxp")

  (take 8 (for [i (range  100000000)
                :let [h (md5 (str test i))]
                :when (= "00000" (subs h 0 5))]
            [(nth h 5) h i (str test i)]
            )) ; 725876aa


  (def res2 (sol2 test))
  (def res2  {\4 \3, \2 \c, \1 \2, \5 \0, \7 \5, \0 \f, \6 \e, \3 \7 })
  (apply str (for [i [\0 \1 \2 \3 \4 \5 \6 \7]]
    (res2 i)))
  (take 8 (for [i (range)
                :let [h (md5 (str test i))]
                :when (= "00000" (subs h 0 5))]
            [(nth h 5) h i (str test i)]
            )) ; 725876aa

  (last (take-while #(not= "00000" (subs  0 5)) 
                    (range 10000000)))  ; 1294456
; 1294456
  (last (take-while #(not= "00000" (subs (md5 (str test %)) 0 5)) 
                    (range 1294457 100000000))) ; nil
; nil
; nil
  (last (take-while #(not= "00000" (subs (md5 (str test %)) 0 5)) 
                    (range 10000000))) 
  (last (take-while #(not= "00000" (subs (md5 (str test %)) 0 5)) 
                    (range 10000000))) 
  (last (take-while #(not= "00000" (subs (md5 (str test %)) 0 5)) 
                    (range 10000000))) 
  (last (take-while #(not= "000000" (subs (md5 (str test %)) 0 6)) 
                    (range 10000000))) ; 9962623
  (md5 (str test 282748))
  (md5 (str test 282749))
  (md5 (str test 9962623))
  (md5 (str test 9962624))
  )
