(ns y-2016.d3
  (:require
    [clojure.string :as str]))


(defn parse-int-line
  [line]
  (map read-string (str/split (str/trim line) #" +")))


(defn triangle?
  [[a b c]]
  (and (< a (+ b c))
       (< c (+ b a))
       (< b (+ a c))))


(defn transpose
  [[a1 b1 c1] [a2 b2 c2] [a3 b3 c3]]
  [[a1 a2 a3] [b1 b2 b3] [c1 c2 c3]])


(comment 
  ;; Rich comment
  (def day 3)
  (def test-file (str "resources/2016/" day))

  (def lines (str/split-lines (slurp test-file)))
  (parse-int-line (first lines))

  (count (filter triangle?
      (map parse-int-line lines))) ; 869

  (count (filter triangle?
      (map parse-int-line lines))) ; 869
  (->> lines
       (map parse-int-line)
       (partition 3)
       (mapcat #(apply transpose %))
       (filter triangle?)
       (count)
       ) ; 1544
  (apply transpose (first  (partition 3 (map parse-int-line lines)))) ; ((810 679 10) (783 255 616) (545 626 626))


  (apply + (map #( Math/abs %) (first res1)))
  (apply + (map #( Math/abs %) (first res2)))
  )
