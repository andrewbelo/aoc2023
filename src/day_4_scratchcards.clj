(ns day-4-scratchcards
  (:require
    [clojure.set :as cset]
    [clojure.string :as str]))


;; Parsing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn parse-number-sets
  [line]
  (->> line
       (map str/trim)
       (map #(str/split % #" +"))
       (map #(map read-string %))
       (map #(apply hash-set %))))


(defn card
  [line]
  (-> line
      (str/split #":")
      (second)
      (str/split  #"\|")
      (parse-number-sets)))


(defn cards
  [text]
  (->> text
       (str/split-lines)
       (map card)))


;; Part 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn calc-matches
  [[wining card]]
  (count (cset/intersection card wining)))


(defn calc-score
  [[wining card]]
  (let [matched (calc-matches [wining card])]
    (case matched
      0 0
      (apply * (repeat (dec matched) 2)))))


(defn result
  [text]
  (->> text
       (cards)
       (map calc-score)
       (apply +)))


(def test-data (slurp "resources/day_4.txt"))
(result test-data) ; 26443


;; Part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn multiply-cards
  ([[times-earned cards] current-card]
   (let [cards-earned  (cards current-card)
         already-have  (times-earned current-card)
         new-cards     (map #(+ 1 current-card %) (range cards-earned))
         safe-inc      #(if (nil? %) nil (+ already-have %))
         new-times     (reduce #(update %1 %2 safe-inc) times-earned new-cards)]
     [new-times cards]))
  ([cards]
   (let [cards-count  (count cards)
         times-earned (into [] (repeat cards-count 1))
         cards-indeces (range cards-count)]
     (first (reduce multiply-cards [times-earned cards] cards-indeces)))))


(defn result-2
  [text]
  (->> text
       (cards)
       (map calc-matches)
       (apply vector)
       (multiply-cards)
       (apply +)))


(result-2 test-data) ; 6284877


(comment
  ;; Testing part 1
  (def example
    "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")
  (def line [" 41 48 83 86 17 " " 83 86  6 31 17  9 48 53"])
  (get (apply vector (map calc-matches (cards example)) ) 0)
  (card (first(str/split-lines example)))
  (map #(str/split % #" +")(map str/trim line))
  (->> line
       (map str/trim)
       (map #(str/split % #" "))
       (map #(map read-string %))
       (map #(apply hash-set %)))

  (result example) ; 13


  ;; Testing part 2
  (result-2 example) ; 30

  )
