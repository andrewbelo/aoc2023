(ns day-17-clumsy
  (:require
    [clojure.pprint :as pprint]
    [clojure.string :as str]))


(def dir-meaning
  {:east [0 1], :west [0 -1], :south [1 0], :north [-1 0]})


(def turns
  {:east [:south :north]  :west [:south :north]
   :south [:west :east]   :north [:west :east]})


(defn txt->nodes
  [text]
  (mapv
    #(mapv read-string (str/split % #""))
    (str/split-lines text)))


(defn visited-start
  [visited]
  (for [dir [:east :south]
        mov [1 2 3]
        :let [coord [0 0 dir mov]]
        :when (contains? visited coord)]
    (visited coord)))


(defn finished?
  [queue visited]
  (let [q-visited     (filter some? (map visited (map drop-last queue)))
        min-queued    (if (empty? q-visited) nil (apply min q-visited))
        start-values  (visited-start visited)
        _ (println start-values)
        min-start     (if (empty? start-values) nil (apply min start-values))
        finished (and (some? min-queued)
                      (some? min-start)
                      (> min-queued min-start))
        _   (println "finished?" min-start min-queued finished)]
    finished))


(defn reachable?
  [[row col dir mov] city visited]
  (let [[new-r new-c] (map - [row col] (dir-meaning dir))
        rows (dec (count city))
        cols (dec (count (first city)))]
    (or (= 0 row col)
        (and (= rows row) (= cols col))
        (and (<= 0 row rows) (<= 0 new-r rows)
             (<= 0 col cols) (<= 0 new-c cols)
             (< mov 3)
             (nil? (visited [row col dir mov]))))))


(defn move
  [[row col dir mov origin-value] city visited]
  (let [[new-r new-c] (map - [row col] (dir-meaning dir))
        turn-ns       (for [new-dir (turns dir)]
                        [new-r new-c new-dir 1])
        all-ns        (if (>= mov 3)
                        turn-ns
                        (conj turn-ns [new-r new-c dir (inc mov)]))
        reachables    (filter #(reachable? % city visited) all-ns)
        new-value     (when (seq reachables) (+ origin-value ((city new-r) new-c)))]
    (map #(conj % new-value) reachables)))


(defn node-heat-loss
  [[row col dir mov origin] city visited]
  (let [[new-r new-c] (map + [row col] (dir-meaning origin))
        next-dirs     (if (= mov 2) (turns dir)
                          (conj (turns dir) dir))
        next-nodes    (for [dir next-dirs
                            mov [0 1 2 3]
                            :let [coord [new-r new-c dir mov]]
                            :when (visited coord)]
                        (visited coord))
        curr-value    ((city row) col)
        next-node-min (if (empty? next-nodes) 0 (apply min next-nodes))]
    (+ curr-value next-node-min)))


(defn factory-value
  [city-map]
  ((city-map (dec (count  city-map))) (dec (count city-map))))


(defn solv
  [city-map max]
  (loop [queue   [[12 12 :south 0 (factory-value city-map)]
                  [12 12 :east 0  (factory-value city-map)]]
         visited  {[12 12 :south 0] (factory-value city-map)
                   [12 12 :east 0] (factory-value city-map)}
         i 0]
    (if (or  (>= i max) (finished? queue visited))
      visited
      (let [new-queue   (mapcat #(move % city-map visited) queue)
            new-visited (into visited (map (juxt drop-last last) new-queue))
            _ (println "--LOOP " i "LET:" (count  visited) (count queue))]
        (recur new-queue
               new-visited
               (inc i))))))


(comment 
  ;; Rich comment
  (def day 17)
  (def test-data (slurp (str "resources/day_" day ".txt")))
  (def exam-data (slurp (str "resources/day_" day "_ex.txt")))

  (count exam-data)
  (count (first exam-data))
  (def factory-node ((exam-data 12) 12))
  (not (seq nil))
  (def city-map (txt->nodes exam-data))
  (def solution (solv city-map 30))
  (def ff (takt 20 (sort  (into #{} 
                             (map #((juxt first second) (first %)) solution)))))

  (take 20 (sort-by #(+ (second (first %)) (ffirst %)) solution))
  )
