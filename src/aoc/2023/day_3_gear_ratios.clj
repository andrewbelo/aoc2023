(ns day-3-gear-ratios
  (:require
    [clojure.set :as cset]
    [clojure.string :as str]))


(def digit? #(<= (int \0) (int %) (int \9)))


(defn symbol-type
  [ch]
  (cond
    (digit? ch)   :digit
    (= ch \.)     :space
    :else         :special))


;; Parsing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn parse-group
  [group]
  (let [{:keys [j sym]} (first group)]
    {:type  (symbol-type sym)
     :j     j
     :val   (str/join (map :sym group))}))


(defn en-rich-number
  "Convert string numbers to int and create additional positions for a number (for each digit)
  First digit is preserved, as id of the number"
  [{pos :pos value :val}]
  (let [move-right  (fn [[x y] i] [x (+ y i)])
        positions   (for [i (range (count value))
                          :let [position  (move-right pos i)
                                num-value (read-string value)]]
                      {position [num-value pos]})]
    (into {} positions)))


;; better with group-by
(defn parse-line-indexed
  "
  Returns a map with the parsed line, grouped by type
  Inserts index of the line into node coordinates
  Each number's position is the position of the first digit
  Gears are counted as symbols

  Results for first two lines of example:
{:numbers
 ({[0 0] [467 [0 0]], [0 1] [467 [0 0]], [0 2] [467 [0 0]]}
  {[0 5] [114 [0 5]], [0 6] [114 [0 5]], [0 7] [114 [0 5]]}),
 :symbols (),
 :gears ()}
{:numbers (), :symbols ([1 3]), :gears ([1 3])}
  "
  [line i]
  (let [symbols       (map #(hash-map :j %1 :sym %2) (range) line)
        line-groups   (partition-by #(symbol-type (:sym %)) symbols)
        parsed-groups (map parse-group line-groups)
        grouper       (fn [group] {:pos [i (:j group)] :val (:val group)})
        type-filter   #(fn [x] (= (:type x) %))
        numbers       (filter (type-filter :digit) parsed-groups)
        symbols       (filter (type-filter :special) parsed-groups)]
    {:numbers (map en-rich-number (map grouper numbers))
     :symbols (map :pos (map grouper symbols))
     :gears   (map :pos (filter #(= "*" (:val %)) (map grouper symbols)))}))


(defn parse-engine
  [text]
  (let [{:keys [numbers symbols gears]}
        (apply merge-with into
               (map parse-line-indexed (str/split-lines text) (range)))]
    {:numbers (into {} numbers)
     :symbols (into #{} symbols)
     :gears (into #{} gears)}))


;; Part 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def neighbours (for [x [-1 0 1] y [-1 0 1] :when (not= 0 x y)] [x y]))


(defn part-numbers-for-symbol
  [numbers sym-pos]
  (let [neighs (map #(map + sym-pos %) neighbours)]
    (into #{} (filter some? (map #(numbers %) neighs)))))


(defn part-numbers
  [numbers symbols]
  (apply cset/union (map (partial part-numbers-for-symbol numbers) symbols)))


(defn result
  [text]
  (let [engine  (parse-engine text)
        parts   (part-numbers (:numbers engine) (:symbols engine))]
    (apply + (map first parts))))


(def test-data (slurp "resources/day_3.txt"))
(result test-data)


;; Part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gear-number-for-symbol
  [numbers gear-pos]
  (let [parts (part-numbers-for-symbol numbers gear-pos)]
    (when (= 2 (count parts))
      (apply * (map first parts)))))


(defn gear-numbers
  [numbers gears]
  (filter some? (map (partial gear-number-for-symbol numbers) gears)))


(defn result-2
  [text]
  (let [engine  (parse-engine text)
        gears   (gear-numbers (:numbers engine) (:gears engine))]
    (apply + gears)))


(result-2 test-data)


(comment
  ;; Testing part 1
  (def example
"467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")

  (def res (parse-engine example) )
  (part-numbers (:numbers res) (:symbols res))
  (parse-line-indexed (nth (str/split example #"\n") 0) 0)
  (parse-line-indexed (nth (str/split example #"\n") 1) 1)
  (result example)

  ;; Testing part 2
  (result-2 example)
  )
