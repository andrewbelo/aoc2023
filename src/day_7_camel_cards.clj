(ns day-7-camel-cards
  (:require
    [clojure.string :as str]))


;; Part 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn hand-type
  [hand-freqs]
  (let [freqs-len (count hand-freqs)
        max-freq  (apply max (vals hand-freqs))]
    (case freqs-len
      1 :five-of-a-kind
      2 (if (= 4 max-freq) :four-of-a-kind :full-house)
      3 (if (= 2 max-freq) :two-pair :three-of-a-kind)
      4 :one-pair
      5 :high-card)))


(defn hand-type-part-1
  [hand]
  (hand-type (frequencies hand)))


(defn hand
  [hand-typer line]
  (let [[cards bid] (str/split line #" ")
        cards-split  (str/split cards #"")]
    {:hand cards-split
     :bid (read-string bid)
     :type (hand-typer cards-split)}))


(defn hands
  [text]
  (map (partial hand hand-type-part-1) (str/split-lines text)))


(defn sort-hands
  [cards-order types-order hands-vec]
  (let [enumerate   #(into {} (map vector % (range)))
        card-values (enumerate cards-order)
        type-values (enumerate types-order)
        sorter      (fn [hand]
                      [(type-values (:type hand))
                       (mapv #(card-values %) (:hand hand))])]
    (sort-by sorter (comp - compare) hands-vec)))


(defn add-rank
  [hands]
  (map (fn [hand rank] (assoc hand :rank (inc rank))) hands (range)))


(defn total-rank
  [hands]
  (reduce #(+ %1 (* (:bid %2) (:rank %2))) 0 hands))


(def cards-order ["A" "K" "Q" "J" "T" "9" "8" "7" "6" "5" "4" "3" "2"])


(def types-order
  [:five-of-a-kind :four-of-a-kind :full-house :three-of-a-kind
   :two-pair :one-pair :high-card])


(defn result
  [text]
  (->> text
       (hands)
       (sort-hands cards-order types-order)
       (add-rank)
       (total-rank)))


;; Part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn move-joker
  [hand-freqs]
  (let [jokers    (hand-freqs "J")
        without-j (dissoc hand-freqs "J")
        [max-val max-freq]  (first (sort-by second > without-j))]
    (if (= 5 jokers) {2 5}
        (assoc without-j max-val (+ max-freq jokers)))))


(defn hand-type-part-2
  [hand]
  (let [hand-freqs  (frequencies hand)
        has-joker?  (contains? hand-freqs "J")
        new-freqs   (if-not has-joker? hand-freqs (move-joker hand-freqs))]
    (hand-type new-freqs)))


(defn hands-2
  [text]
  (map (partial hand hand-type-part-2) (str/split-lines text)))


(def cards-order-2 ["A" "K" "Q" "T" "9" "8" "7" "6" "5" "4" "3" "2" "J"])


(defn result-2
  [text]
  (->> text
       (hands-2)
       (sort-hands cards-order-2 types-order)
       (add-rank)
       (total-rank)))


(comment
  ;; Testing
  ;; (def example-data (slurp (str "resources/day_" day "_ex.txt" )))
  (def example
"32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
")

  (hand hand-type-part-2  "J2J79 991")
  (hands example)
  (sort-hands cards-order types-order example)
  (result example)
  (hands-2 example)
  (result-2 example) ; 5905

  (def day 7)
  (def test-data (slurp (str "resources/day_" day ".txt")))
  (hands test-data)
  (result test-data) ; 250898830
  (result-2 test-data)

  (result test-data) ; 250898830
  (result-2 test-data)
  )
