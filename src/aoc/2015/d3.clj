(ns y-2015.d3
  (:require
    [clojure.set :as set]
    [clojure.string :as str]))


(comment 
  ;; Rich comment
  (def day 3)
  (def test-file (str "resources/2015/" day))

  (def res (reduce (fn [[[r c] visited] dir]
                     (let [pos' (case dir
                                  \> [r (inc c)]
                                  \< [r (dec c)]
                                  \^ [(dec r) c]
                                  \v [(inc r) c]
                                  )]
                       [pos' (assoc visited pos' (inc (visited pos' 0)))])
                     )
          [[0 0] {[0 0] 1}] 
          (str/trim (slurp test-file))))

  (defn move [[r c] dir]
    (case dir
      \> [r (inc c)]
      \< [r (dec c)]
      \^ [(dec r) c]
      \v [(inc r) c]
      0  [r c]))

  (def res2 (reduce (fn [[pos visited rpos rvisited] [dir rdir]]
                      (let [pos'  (move pos dir)
                            rpos' (move rpos rdir)]
                        [pos'   (into visited [pos'])
                         rpos'  (into rvisited [rpos'])])
                      )
          [[0 0] #{[0 0]} [0 0] #{[0 0]}] 
          (partition 2 (concat (str/trim (slurp test-file)) "0"))))

  (count (second res)) ; 2592
  (+  (count (second res2)) (count (last res2))) 
  (count  (into (second res2) (last res2))) ; 2360

  (into #{1 2} #{2 3})
  (concat "fdkjfda" "1")
  (partition 2 2 [0] [1 2 3])
  (count (slurp test-file))
  )
