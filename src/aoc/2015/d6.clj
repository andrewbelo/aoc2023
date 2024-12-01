(ns y-2015.d6
  (:require
    [clojure.set :as set]
    [clojure.string :as str]))


(defn line->
  [line]
  (let [parts         (str/split line #" ")
        turn-meaning  {"on" true "off" false}
        [from _ to]   (drop (- (count parts) 3) parts)
        [from-c to-c] (mapv #(mapv read-string (str/split % #",")) [from to])
        [action dir]  (case (first parts)
                        "toggle"  [:toggle 2]
                        "turn"    [:turn  (turn-meaning (second parts))])]
    [[action dir] from-c to-c]))


(defn perform
  [visited [[action dir] [from-r from-c] [to-r to-c]]]
  (let [actions {:turn (fn [_] dir) :toggle not}
        op      (actions action)]
    (into visited  (for [r (range from-r (inc to-r))
                         c (range from-c (inc to-c))]
                     [[r c] (op (visited [r c] false))]))))


(defn perform-2
  [visited [[action dir] [from-r from-c] [to-r to-c]]]
  (let [actions {:turn    #(max 0 (+ % (if dir 1 -1)))
                 :toggle  #(+ % dir)}]
    (into visited  (for [r (range from-r (inc to-r))
                         c (range from-c (inc to-c))]
                     [[r c] ((actions action) (visited [r c] 0))]))))


(comment 
  ;; Rich comment
  (def day 6)
  (def test-file (str "resources/2015/" day))


  (line-> "toggle 537,781 through 687,941")
  (line-> "turn on 226,196 through 599,390")
  (line-> "turn off 226,196 through 599,390")

  (def commands (map line-> (str/split-lines (slurp test-file))))

  (def res (reduce perform {} commands))
  (count (filter second res)) ; 733456 733456 400410

  (def res2 (reduce perform-2 {} commands))
  (apply + (map second res2)) ; 15343601

  )
