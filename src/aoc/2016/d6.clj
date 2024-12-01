(ns y-2016.d5
  (:require
    [clj-commons.digest :refer [md5]]
    [clojure.set :as set]
    [clojure.string :as str]))


(defn columns
  [lines]
  (reduce
    (fn [cols line]
      (into [] (for [i (range 8)]
                 (conj (cols i) (nth line i)))))
    (into [] (repeat 8 []))
    lines))


(comment 
  ;; Rich comment
  (def day 6)
  (def test-file (str "resources/2016/" day))
  (def lines  (str/split-lines (slurp test-file)))
  (count (first lines))

  (map (comp
         #(take 5 %)
         #(sort-by second > %)
          frequencies) 
       (columns lines))
  )
