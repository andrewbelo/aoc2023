(ns day-14-parabolic-dish
  (:require
    [clojure.set :as set]
    [clojure.string :as str]))


(defn parse-columns
  [text]
  (let [input-map     (mapv #(str/split % #"") (str/split-lines text))
        rocks         (for [r (range (count input-map))
                            c (range (count (first input-map)))
                            :let [rock (get-in input-map [r c])]
                            :when (#{"#" "O"} rock)]
                        [[r c] rock])
        columns       (group-by (comp second first) rocks)
        flatten-rocks (fn [[_ val]] (mapv first val))
        rocks-meaning {"#" :unmovable, "O" :movable}]
    (into {} (map (juxt key (comp
                              #(into {} %)
                              #(mapv (juxt (comp rocks-meaning key) flatten-rocks) %)
                              #(group-by second %)
                              #(map (juxt ffirst second) %)
                              val))
                  columns))))


(defn find-closest-unmovable
  [unmovable rock]
  (apply max -1 (remove #(> % rock) unmovable)))


(defn move-rocks-north
  [[_ {:keys [movable unmovable]} :as rocks]]
  (assoc-in rocks [1 :movable]
            (->> movable
                 (map (partial find-closest-unmovable unmovable))
                 (group-by identity)
                 (map (fn [[_ rocks]] (mapv + rocks (range 1 (inc (count rocks))))))
                 (flatten)
                 (into []))))


(defn pivot
  ([north-value columns]
   (let [[mov unmov] (mapv #(pivot north-value columns %) [:movable :unmovable])]
     (merge-with merge mov unmov)))
  ([north-value columns rock-type]
   (let [pivot-fn    (fn [[col rows]]  (map #(vector col (- north-value % 1)) rows))]
     (->> columns
          (map (juxt key (comp rock-type val)))
          (mapcat pivot-fn)
          (group-by second)
          (map (juxt key (comp #(hash-map rock-type %) #(mapv first %) val)))
          (into {})))))


(defn cycle-rocks
  ([north-value columns _]
   (pivot north-value (into {} (map move-rocks-north columns))))
  ([north-value columns]
   (reduce (partial cycle-rocks north-value) columns (range 4))))


(defn cycle-till-repeat
  ([north-value {:keys [cycles latest]} c]
   (let [new-rocks (cycle-rocks north-value latest)]
     (if-let [first-cycle (cycles new-rocks)]
       (reduced [(set/map-invert cycles) first-cycle c])
       {:cycles (assoc cycles new-rocks c), :latest new-rocks})))
  ([north-value columns]
   (reduce (partial cycle-till-repeat north-value)
           {:cycles {columns 0}, :latest columns}
           (range 1 1000000))))


(defn result-2
  [north-value columns]
  (let [[cycles fst sec]  (cycle-till-repeat north-value columns)
        cycle-len         (- sec fst)
        billionth         (rem (- 1000000000 fst 1) cycle-len)]))


(defn final-calc
  [part text]
  (let [columns     (parse-columns text)
        north-value (count (str/split-lines text))]
    (case part
      1 (->> columns
             (map move-rocks-north))
      2 (cycle-till-repeat north-value columns))))


(defn print-rocks
  [columns size]
  (let [chs (for [r (range size)
                  c (range size)
                  :let [col (get columns c)
                        mov (:movable col)
                        unmov (:unmovable col)]]
              (cond
                (some #(= % r) mov)   \O
                (some #(= % r) unmov) \#
                :else                 \.))]
    (->> chs
         (partition size)
         (map str/join)
         (str/join "\n"))))


(comment
  ;; Testing
  (def example
"O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....")

  (def columns (parse-columns example))
  (let [all-movable (map (juxt key (comp :movable val)) columns )
        pivot-fn    (fn [[col rows]]  (map #(vector col %) rows)) ]
    (mapcat pivot-fn all-movable))

  (map (juxt key (comp  val)) (pivot columns :movable))
  (pivot columns :unmovable)
  (pivot columns 10)
  (cycle-rocks 10 columns)
  (print (print-rocks columns 10))

  (print (print-rocks (cycle-rocks 10 columns ) 10))
  (cycle-till-repeat 10 columns)

  (apply +(map (partial move-rocks-north (count (str/split-lines example))) columns))

  (final-calc 1 example)
  (final-calc 2 example)
  (defn billionth [fst sec] (rem (- 1000000000 fst 1) (- sec fst)))
  (billionth 3 190)

  (def day 14)
  (def test-data (slurp (str "resources/day_" day ".txt")))
  (count (str/split-lines test-data))

  (final-calc 1 test-data)
  (def res (final-calc 2 test-data))
  88 190
  (apply + (map (comp (fn [col] (apply +(map #(- 100 %) col))) 
             :movable val) 
       ((first res) 71)))
  (rem (- 1000000000 89) (- 190 88))
  (time (final-calc 1 test-data))
  (time (final-calc 2 test-data))
  )
