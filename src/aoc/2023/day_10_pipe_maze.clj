(ns day-10-pipe-maze
  (:require
    [clojure.set :as set]
    [clojure.string :as str]))


(defn pipes
  [text]
  (->> text
       (str/split-lines)
       (mapv #(str/split % #""))
       (mapv vec)
       (apply vector)))


(defn connect-pipes
  [free-conn new-pipe-conns]
  (let [[l-diff r-diff] (mapv #(mapv + free-conn %) new-pipe-conns)]
    (cond
      (= l-diff [0 0]) (second new-pipe-conns)
      (= r-diff [0 0]) (first new-pipe-conns)
      :else             nil)))


(def pipes-map
  {\| [[+1 +0] [-1 +0]]
   \- [[+0 +1] [+0 -1]]
   \L [[+0 +1] [-1 +0]]
   \J [[+0 -1] [-1 +0]]
   \7 [[+0 -1] [+1 +0]]
   \F [[+0 +1] [+1 +0]]
   \S [[+0 +1] [+1 +0] [+0 -1] [-1 +0]]})


(defn node-meaning
  [pipes node]
  (get-in pipes-map (get-in pipes node ".")))


(defn node-connections
  [pipes node]
  (->> (node-meaning pipes node)
       (mapv #(vector % (node-meaning pipes (mapv + node %))))))


(defn start-node
  [pipes]
  (first (for [r (range (count pipes))
               c (range (count (first pipes)))
               :when (= (get-in pipes [r c]) "S")]
           [r c])))


(defn start
  [pipes]
  (let [start-node (start-node pipes)
        start-conns (node-connections pipes start-node)]
    (->> start-conns (mapv #(apply connect-pipes %))
         (mapv (fn [diff conns] [(mapv + start-node diff) conns])
               (map first start-conns))
         (remove #(nil? (second %)))
         (map vector))))


(defn ends-meet?
  [[lhs-old rhs-old] [lhs-new rhs-new]]
  (or (= lhs-new rhs-new)
      (= lhs-new rhs-old)
      (= rhs-new lhs-old)))


(defn next-pipe
  [pipes curr-pipe free-conn]
  (let [next-node (mapv + curr-pipe free-conn)
        next-pipe-conns (node-meaning pipes next-node)]
    (when-let [next-conn (connect-pipes free-conn next-pipe-conns)]
      [next-node next-conn])))


(defn move-pipes
  [pipes chains i]
  (let [ends      (mapv last chains)
        new-ends  (map #(apply next-pipe pipes %) ends)
        new-chain (mapv conj chains new-ends)]
    (if (ends-meet? (mapv first ends) (mapv first new-ends))
      (reduced [i new-chain])
      new-chain)))


(defn sums-so-far
  [sums score]
  (if-let [last-sum (last sums)] (conj sums (+ score last-sum)) [score]))


(defn count-inside-row
  [pipes [_ row]]
  (let [unique-row  (sort (into #{} row))
        letters     (map #(get-in pipes %) unique-row)
        wall-values {"-" 0, "|" 1, "L" -0.5, "J" 0.5, "7" -0.5, "F" 0.5, "S" 0}
        scores      (reduce sums-so-far [] (mapv wall-values letters))
        inside-out  (map vector (map second unique-row) scores)]
    (->>  inside-out
          (map (fn [[_ score]] [_ (= 1 (mod (int score) 2))]))
          (partition 2 1)
          (map (fn [[[a b] [c _]]] [(- c a 1) b]))
          (remove #(= 0 (first %)))
          (filter second)
          (map first)
          (apply +))))


(defn find-insides
  [pipes pipe-loop]
  (->> pipe-loop
       (group-by first)
       (sort)
       (drop 1)
       (drop-last)
       (map (partial  count-inside-row pipes))
       (apply +)))


(defn final-calc
  [part text]
  (let [ps (pipes text)
        result-1 (reduce (partial move-pipes ps) (start ps) (range))
        pipe-loop (apply concat (map #(map first %) (second result-1)))]
    (case part
      1 (/ (count pipe-loop) 2)
      2 (find-insides ps (conj pipe-loop (start-node ps))))))


(comment
  ;; Testing
  (def example
    "FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L")

  (def ps (pipes example))
  (start-node ps)
  (def starts ( start ps ))
  (def res (reduce (partial  move-pipes ps) starts (range)))
  (first res)
  (map #(vector (first % ) (second %)(get-in ps (reverse (second %)))) starts)
  (node-connections ps [1 1])
  (node-connections ps [1 2])
  
  (get-in pipes-map (get-in ps [1 1]))
  (get-in pipes-map "-")

  (final-calc 1 example)
  (final-calc 2 example)
  (def example-loop (final-calc 2 example))
  (find-insides (pipes example) example-loop)
  (->> example-loop
       (group-by first)
       (sort-by first)
       (map (partial count-inside-row (pipes example)))
       ;; (take 2)
       )
  (first example-loop)

  (def day 10)
  (def test-data (slurp (str "resources/day_" day ".txt")))
  (def test-pipes (pipes test-data))
  (get-in (pipes test-data) [84 25])
  (get-in (pipes test-data) [83 24])
  (count (pipes test-data))
  (def result (final-calc 1 test-data))
  (def pipe-loop (apply concat(map #(map first % ) (second result))))

  (count-inside-row (nth (sort (group-by first pipe-loop)) 3))

  (map second (second (first (group-by first pipe-loop))))

  (def w (apply map vector (map #(map first % ) (second result))))
  (count (map #(get-in test-pipes %) (map first w)))
  (get-in ps [84 25])


  (final-calc 1 test-data)
  (final-calc 2 test-data)
  (time (final-calc 1 test-data))
  (time (final-calc 2 test-data))

  )
