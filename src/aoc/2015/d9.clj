(ns y-2015.d9
  (:require
    [clojure.data.priority-map :refer [priority-map]]
    [clojure.set :as set]
    [clojure.string :as str]))


(defn line->node
  [line]
  (let [[lhs dst] (str/split line #" = ")
        [c-l c-r] (map keyword (str/split lhs #" to "))]
    [[c-l [c-r (read-string dst)]]
     [c-r [c-l (read-string dst)]]]))


(defn lines->nodes
  [lines]
  (into {} (map (juxt first
                      (fn [[_ lst]]
                        (into {} (map second lst))))
                (group-by first (mapcat line->node lines)))))


(defn connect-to-beggining
  [[n visited prev-max] curr-len nodes]
  (let [next-n  (first visited)]
    [(conj visited next-n)
     (+ curr-len (min prev-max ((nodes n) next-n)))]))


(defn connect-to-new
  [[n visited prev-max] curr-len nodes]
  (for [next-n  (keys (nodes n))
        :when   (not ((into #{} visited) next-n))
        :let    [len ((nodes n) next-n)]]
    [{:node   next-n
      :visited (conj visited next-n)
      :prev-max (max prev-max len)}
     (+ curr-len (min prev-max len))]))


(defn connect-to-beggining-2
  [[n visited prev-max] curr-len nodes]
  (let [next-n  (first visited)
        len ((nodes n) next-n)]
    [(conj visited next-n)
     (+ curr-len len  (- (min prev-max len)))]))


(defn connect-to-new-2
  [[n visited prev-max] curr-len nodes]
  (for [next-n  (keys (nodes n))
        :when   (not ((into #{} visited) next-n))
        :let    [len ((nodes n) next-n)]]
    [{:node   next-n
      :visited (conj visited next-n)
      :prev-max (if (= 0 prev-max) len  (min prev-max len))}
     (+ curr-len len)]))


(defn search
  [nodes]
  (loop [q (priority-map
             {:node          (first (keys nodes))
              :visited       [(first (keys nodes))]
              :prev-max 0}
             0)
         f #{}
         i 0]
    (let  [[{:keys [node visited prev-max]} dist] (peek q)
           all-nodes    (keys nodes)]
      (cond
        (> i 100000) f
        (empty? q) f

        (= (count visited) (count all-nodes))
        (recur
          (pop q)
          (conj f (connect-to-beggining-2 [node visited prev-max] dist nodes))
          (inc i))

        :else
        (recur
          (into (pop q)
                (connect-to-new-2 [node visited prev-max] dist nodes))
          f
          (inc i))))))


(comment 
  ;; Rich comment
  (def day 9)
  (def lines (str/split-lines (slurp (str "resources/2015/" day))))

  (def nodes (lines->nodes lines))

  (def example 
    "London to Dublin = 464
London to Belfast = 518
Dublin to Belfast = 141")
  (def lines-ex  (lines->nodes (str/split-lines example)))



  (take 10 (sort-by second  (search nodes)))
  (take 10 (sort-by second > (search nodes)))
  (search lines-ex)
  )
