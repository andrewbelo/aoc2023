(ns day-7-camel-cards
  (:require
    [clojure.string :as str]))


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


(defn move-joker
  [hand-freqs]
  (let [jokers              (hand-freqs "J" 0)
        without-j           (dissoc hand-freqs "J")
        [max-val max-freq]  (first (sort-by second > without-j))]
    (case jokers
      0 hand-freqs
      5 {2 5}
      (assoc without-j max-val (+ max-freq jokers)))))


(defn hand
  [task line]
  (let [[cards bid] (str/split line #" ")
        cards-split (str/split cards #"")
        freqs       (frequencies cards-split)]
    {:hand cards-split
     :bid (read-string bid)
     :type (hand-type (if (= task 1) freqs (move-joker freqs)))}))


(defn sort-hands
  [cards-order types-order hands-vec]
  (let [sorter (fn [hand]
                 [(.indexOf types-order (:type hand))
                  (mapv #(.indexOf cards-order %) (:hand hand))])]
    (sort-by sorter (comp - compare) hands-vec)))


(defn add-rank
  [hands]
  (map (fn [hand rank] (assoc hand :rank (* (:bid hand) (inc rank))))
       hands (range)))


;; RESULTS  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn calc-result
  [text task]
  (let [cards-order   ["A" "K" "Q" "J" "T" "9" "8" "7" "6" "5" "4" "3" "2"]
        cards-order-2 ["A" "K" "Q" "T" "9" "8" "7" "6" "5" "4" "3" "2" "J"]
        cards         (case task 1 cards-order cards-order-2)
        types-order   [:five-of-a-kind :four-of-a-kind :full-house
                       :three-of-a-kind :two-pair :one-pair :high-card]]
    (->> text
         (str/split-lines)
         (map (partial hand task))
         (sort-hands cards types-order)
         (add-rank)
         (map :rank)
         (apply +))))


(defn result
  [text]
  (calc-result text 1))


(defn result-2
  [text]
  (calc-result text 2))


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

  (hand 2  "J2J79 991")
  (sort-hands cards-order types-order (hands example))
  (result example) ; 6440
  (hands-2 example)
  (result-2 example) ; 5905

  (def day 7)
  (def test-data (slurp (str "resources/day_" day ".txt")))
  (hands test-data)
  (result test-data) ; 250898830
  (result-2 test-data) ; 252127335

  (time (result test-data))
  ; (out) "Elapsed time: 591.452433 msecs"
  (time (result-2 test-data))
  ; (out) "Elapsed time: 504.570163 msecs"
  )
