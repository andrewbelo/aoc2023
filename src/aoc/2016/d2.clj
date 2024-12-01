(ns y-2016.d2
  (:require
    [clojure.string :as str]))


(def directions {\D [1 0] \U [-1 0] \L [0 -1] \R [0 1]})
(def keypad [[1 2 3] [4 5 6] [7 8 9]])


(def keypad-2
  [[nil nil "1" nil nil]
   [nil "2" "3" "4" nil]
   ["5" "6" "7" "8" "9"]
   [nil  "A" "B" "C" nil]
   [nil nil "D" nil nil]])


(defn determine-buttom
  [line]
  (reduce
    (fn [[r c] dir]
      (let [[r' c'] (mapv + [r c] (directions dir))]
        (if (and (<= 0 r' 2) (<= 0 c' 2)) [r' c'] [r c])))
    [1 1]
    line))


(defn determine-buttom-2
  [line]
  (reduce
    (fn [[r c] dir]
      (let [[r' c'] (mapv + [r c] (directions dir))]
        (if (and (<= 0 r' 4) (<= 0 c' 4)
                 ((keypad-2 r') c')) [r' c'] [r c])))
    [2 0]
    line))


(comment 
  ;; Rich comment
  (def day 2)
  (def test-file (str "resources/2016/" day))

  (def lines (str/split-lines (slurp test-file)))
  (count lines) ; 5
  (determine-buttom "ULL")
  (determine-buttom "RRDDD")
  (def res1 
    (map (fn [[r c]] ( (keypad r) c))
    (map determine-buttom lines))
    )
  (apply str res1) ; "47978"
  (def res2 
    (map (fn [[r c]] ( (keypad-2 r) c))
    (map determine-buttom-2 lines))
    )
  (apply str res2)  ; "659AD"


  (apply + (map #( Math/abs %) (first res1)))
  (apply + (map #( Math/abs %) (first res2)))
  )
