(ns day-16-lava-floor
  (:require
    [clojure.set :as set]
    [clojure.string :as str]))


(defn parse-nodes
  [text]
  (let [meanings {"."  :empty-space
                  "/"  :negative-mirror
                  "\\" :positive-mirror
                  "-"  :horiz-splitter
                  "|"  :verti-splitter}]
    (->> text
         (str/split-lines)
         (map #(str/split % #""))
         (mapv #(mapv meanings %)))))


(defn neighbours
  [node curr-direction]
  (let [splitter (fn [dir changing-coord]
                   (if (zero? (changing-coord dir)) [dir]
                       ((juxt (comp #(into [] %) rseq)
                              (comp #(mapv (partial * -1) %) rseq))
                        dir)))
        meanings
        {:empty-space     (comp vector identity)
         :positive-mirror (comp vector #(into [] %) rseq)
         :negative-mirror (comp vector #(mapv (partial * -1) %) rseq)
         :horiz-splitter  #(splitter % first)
         :verti-splitter  #(splitter % second)}]
    ((meanings node) curr-direction)))


(defn move-to-neighbours
  [current nodes valid?]
  (let [flatten-direction #((partial mapv + (first current)) %)]
    (->> current
         ((juxt (comp #(get-in nodes %) first) second))
         (apply neighbours)
         (mapv (juxt flatten-direction identity))
         (filter valid?)
         (into []))))


(defn neighbour-valid?
  [nodes visited [coords dirs]]
  (and (>= (apply min coords) 0)
       (< (apply max coords) (count nodes))
       (nil? (visited [coords dirs]))))


(defn shoot-laser
  [nodes start]
  (loop [discovered [start], visited #{}]
    (if (empty? discovered) (into #{} (map first visited))
        (let [[current to-discover] ((juxt first next) discovered)
              neighbour-valid?      (partial neighbour-valid? nodes visited)
              neighbours-discovered (move-to-neighbours current
                                                        nodes
                                                        neighbour-valid?)]
          (recur (concat to-discover neighbours-discovered)
                 (conj visited current))))))


(defn final-calc
  [part text]
  (let [nodes (parse-nodes text)
        end (dec (count nodes))
        start (case part
                1 [[[0 0] [0 1]]]
                2 (mapcat #(vector [[0 %] [1 0]]
                                   [[% 0] [0 1]]
                                   [[end %] [-1 0]]
                                   [[% end] [0 -1]])
                          (range (count nodes))))]
    (->> start
         (map #(shoot-laser nodes %))
         (map count)
         (apply max))))


(comment
  ;; Testing
  (def example (slurp "resources/day_16_ex.txt"))

  
  (def example-nodes (parse-nodes example))
  (def example-res (shoot-laser example-nodes))
  (into #{} (map first example-res))
  (def grid (for [i (range (count example-nodes))
                  j (range (count example-nodes))]
              (if (example-res [i j]) \# \.)))
  (defn print-grid [grid size]
    (map str/join (partition  size grid)))
  (print-grid grid 10)
  (get-in (parse-nodes example) [0 0])

  (final-calc 1 example)
  (final-calc 2 example)

  (def day 16)
  (def test-data (slurp (str "resources/day_" day ".txt")))

  (final-calc 1 test-data)
  (final-calc 2 test-data)
  (time (final-calc 1 test-data))
  (time (final-calc 2 test-data))

  )
