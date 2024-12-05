(ns aoc.2024.d05
  (:require
    [clojure.string :as str]))


(def day 5)
(def test-txt (slurp (str "resources/2024/" day)))
(def example-txt (slurp (str "resources/2024/" day "e")))


(defn txt->
  [txt]
  (let [[rules updates] (map str/split-lines (str/split txt #"\n\n"))]
    [(map #(mapv parse-long (str/split % #"\|")) rules)
     (map #(mapv parse-long (str/split % #",")) updates)]))


(defn ordered?
  [rules update]
  (reduce
    (fn [visited page]
      (if (empty? (filter (rules page #{}) visited))
        (conj visited page)
        (reduced false)))
    []
    update))


(defn minimum-left?
  [page left rules]
  (empty? (filter #(get-in rules [page %] nil) left)))


(defn order-update
  [rules update]
  (first
    (reduce
      (fn [[visited left] _]
        (let [page' (first (filter #(minimum-left? % left rules) left))]
          [(conj visited (first page')) (disj left page')]))
      [[] (into #{} update)]
      update)))


(defn sol-1
  [input]
  (let [[rules updates] (txt-> input)
        after-dict (into {} (map (fn [[k v]] [k (into #{} (map second v))])
                                 (group-by first rules)))
        res (filter (partial ordered? after-dict) updates)]
    (apply +  (map #(% (quot (count %) 2)) res))))


(defn sol-2
  [input]
  (let [[rules updates] (txt-> input)
        after-dict (into {} (map (fn [[k v]] [k (into #{} (map second v))])
                                 (group-by first rules)))

        res         (remove (partial ordered? after-dict) updates)
        ordered-res (map (partial order-update after-dict) res)]
    (apply +  (map #(% (quot (count %) 2)) ordered-res))))


(comment
  (def ex (txt-> example-txt))
  (def ps (map str/split-lines (str/split example-txt #"\n\n")))
  (def rules (first ex))

  (def before-dict (into {} (map (fn [[k v]] [k (into #{} (map second v))])
                        (group-by first rules))))
  (def after-dict (into {} (map (fn [[k v]] [k (into #{} (map first v))])
                        (group-by second rules))))
  (ordered? before-dict (first (second ex) ))
  (order-update after-dict [ 97,13,75,29,47])
  (sol-1 example-txt)
  (sol-2 example-txt)

  (time (sol-1 test-txt)) ; 4281

  (time (sol-2 test-txt)) ; 5466
  )
