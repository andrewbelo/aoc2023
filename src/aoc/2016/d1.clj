(ns y-2016.d1
  (:require
    [clojure.string :as str]))


(defn parse-cmd
  [line]
  (let [dir (first line)
        mov (subs line 1)]
    [dir (read-string mov)]))


(def directions
  {[0 1]  {\L [-1 0] \R [1 0]}
   [0 -1] {\L [1 0] \R [-1 0]}
   [1 0]  {\L [0 1] \R [0 -1]}
   [-1 0] {\L [0 -1] \R [0 1]}})


(defn step
  [[pos dir] [dir' mov]]
  (let [new-dir ((directions dir) dir')
        pos'    (map #(+ %1 (* mov %2)) pos new-dir)]
    [pos' new-dir]))


(defn step-2
  [[pos dir visited] [dir' mov]]
  (let [new-dir ((directions dir) dir')
        pos'    (mapv #(+ %1 (* mov %2)) pos new-dir)
        passed  (for [m   (range 1 (inc mov))
                      :let [v (mapv #(* % m) new-dir)]]
                  (mapv + pos v))
        res     [pos' new-dir (into visited passed)]]
    (if (empty? (filter visited passed))
      res
      (reduced (filter visited passed)))))


(comment 
  ;; Rich comment
  (def day 1)
  (def test-file (str "resources/2016/" day))
  (def line (slurp test-file))
  (def line "R8, R4, R4, R8")
  (def cmds  (str/split  (str/trim line) #", "))
  (def parsed-cmds (map parse-cmd cmds))
  (def res1 (reduce 
              step
              [[0 0] [-1 0]]
              parsed-cmds
              ))
  (def res2 (reduce 
              step-2
              [[0 0] [-1 0] #{[0 0]}]
              parsed-cmds
              ))
  (apply + (map #( Math/abs %) (first res1)))
  (apply + (map #( Math/abs %) (first res2)))
  )
