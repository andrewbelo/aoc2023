(ns y-2015.d11
  (:require
    [clojure.data.priority-map :refer [priority-map]]
    [clojure.set :as set]
    [clojure.string :as str]))


(defn next-perm
  [line]
  (let [first-part (subs line 0 (dec (count line)))
        [true-first-part suffix]
        (if (= \z (last line))
          [(next-perm first-part) \a]
          [first-part (char  (inc (int (last line))))])]
    (str true-first-part suffix)))


(defn valid?
  [line]
  (let [straight?     (first (filter (fn [[a b c]] (= c (inc b) (+ 2 a)))
                                     (partition 3 1 (map int line))))
        no-iol?       (nil? (first (filter #{\i \o \l} line)))
        doubles       (filter some?
                              (map-indexed (fn [i [a b]] (when (= b a) i))
                                           (partition 2 1 line)))
        two-double?   (or (> (count doubles) 2)
                          (and (= (count doubles) 2)
                               (> (dec (second doubles)) (first doubles))))]
    (and  straight? no-iol? two-double?)))


(valid? "hijklmmn")
(valid?  "hepxcrrr")
(valid? "abbcegjk")
(valid? "abbceggjk")
(valid? "abbcegjk")


;; Passwords must include one increasing straight of at least three letters,
;; NO i, o, or l
;; Passwords must contain at least two different, non-overlapping pairs of letters,

(char (inc (int \a)))


(comment 
  ;; Rich comment
  (def day 11)
  (def line "hepxcrrq")

  (next-perm "zzzz")
  (def line2 "hepxxyzz")

  (reduce (fn [line i] 
            (if (valid? line)
              (reduced [i line])
              (next-perm line)))
          (next-perm line)
          (range)
          ) 
  (reduce (fn [line i] 
            (if (valid? line)
              (reduced [i line])
              (next-perm line)))
          (next-perm line2)
          (range)
          ) 
  ;; 97 -- 122
  (int \a)
  (int \z)
  ; (out) "Elapsed time: 712.880917 msecs"
  ; 360154
  ; eval (root-form): (time (res line 50))
  ; (out) "Elapsed time: 9969.506103 msecs"
  ; 5103798


  (take 10 (sort-by second  (search nodes)))
  (take 10 (sort-by second > (search nodes)))
  (search lines-ex)
  )
