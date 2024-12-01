(ns y-2015.d5
  (:require
    [clojure.set :as set]
    [clojure.string :as str]))


;; It contains at least three vowels
;;  aeiou 
;; It contains at least one letter that appears twice in a row
;; It does not contain the strings ab, cd, pq, or xy
(defn nice?
  [line]
  (let [vowel-count   (count (filter #{\a \e \i \o \u} line))
        repeats-count (apply max (map count (partition-by identity line)))
        forbidden     (map #(str/includes? line %) ["ab" "cd" "pq" "xy"])]
    (and (>= vowel-count 3)
         (>= repeats-count 2)
         (every? false? forbidden))))


;; It contains a pair of any two letters that appears at least twice in the string
;; without overlapping
;; It contains at least one letter which repeats with exactly one letter between them
(defn nice2?
  [line]
  (let [pair-repeat (->> line
                         (partition 2 1)
                         (zipmap (range))
                         (group-by second)
                         (map second)
                         (filter #(or (> (count %) 2)
                                      (and (= (count %) 2)
                                           (< 1 (Math/abs (- (ffirst %)
                                                             (first (second %))))))))
                         (seq))
        chaperon-repeat (seq (filter (fn [[a _ c]] (= a c)) (partition 3 1 line)))]
    (and pair-repeat chaperon-repeat)))


(comment 
  ;; Rich comment
  (def day 5)
  (def test-file (str "resources/2015/" day))

  (nice? "ugknbfddgicrmopn")
  (nice? "aaa")
  (nice? "jchzalrnumimnmhp")
  (nice? "haegwjzuvuyypxyu")
  (nice? "dvszwmarrgswjxmb")
; ugknbfddgicrmopn is nice 
; aaa is nice
; jchzalrnumimnmhp is naughty because it has no double letter.
; haegwjzuvuyypxyu is naughty because it contains the string xy.
; dvszwmarrgswjxmb is naughty because it contains only one vowel.
  (count  (filter nice? (str/split-lines (slurp test-file))))
  (count  (filter nice2? (str/split-lines (slurp test-file))))

  (nice2? "qjhvhtzxzqqjkmpb")
  (nice2? "xxyxx")
  (nice2? "uurcxstgmygtbstg")
  (nice2? "ieodomkazucvgmuy")

; qjhvhtzxzqqjkmpb is nice 
; xxyxx is nice 
; uurcxstgmygtbstg is naughty 
; ieodomkazucvgmuy is naughty 
  )
