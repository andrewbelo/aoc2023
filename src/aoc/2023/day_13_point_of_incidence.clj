(ns day-13-point-of-incidence
  (:require
    [clojure.set :as set]
    [clojure.string :as str]))


(defn parse-rocks
  [text]
  (let [rocks         (mapv #(str/split % #"") (str/split-lines text))
        rocks-coords  (for [i (range (count rocks))
                            j (range (count (first rocks)))
                            :when (= "#" (get-in rocks [i j]))]
                        [i j])
        separator     (fn [row? coords]
                        (let [[f s] (if row? [first second] [second first])]
                          (->> coords
                               (group-by f)
                               (mapv (juxt key (comp sort #(mapv s %) val)))
                               (into {}))))]
    {:rows (separator true rocks-coords)
     :cols (separator false rocks-coords)
     :dim-y (count rocks)
     :dim-x (count (first rocks))}))


(defn mirrored-rocks-freqs
  [rows mirror-max mirror]
  (for [row rows]
    (->> row
         (map #(Math/abs (- % mirror -0.5)))
         (drop-while #(> % mirror-max))
         (take-while #(<= % mirror-max))
         (frequencies) (vals))))


(defn not-mirror?
  [mirror-check rows size mirror]
  (let [mirror-max    (min mirror (- size mirror))
        mirrored-rows (mirrored-rocks-freqs rows mirror-max mirror)]
    ((complement mirror-check) (apply concat mirrored-rows))))


(defn find-1-mirror
  [mirror-check line size]
  (let [found (first (drop-while #(not-mirror? mirror-check (vals line) size %)
                                 (range 1 size)))]
    (if (nil? found) 0 found)))


(defn find-mirrors
  [mirror-check {:keys [rows cols dim-x dim-y]}]
  (let [[vertical horizontal] (map #(find-1-mirror mirror-check %1 %2)
                                   [rows cols]
                                   [dim-x dim-y])]
    (+ (*  horizontal 100) vertical)))


(defn final-calc
  [part text]
  (let [mirror-check (case part
                       1 (fn [freqs] (every? #{2} freqs))
                       2 (fn [freqs]
                           (and (every? #{2 1} freqs)
                                (= 1 (count (take 2 (filter #{1} freqs)))))))
        parts (str/split text #"\n\n")]
    (->> parts
         (map parse-rocks)
         (map #(find-mirrors mirror-check %))
         (apply +))))


(comment
  ;; Testing
  (def example (slurp "resources/day_13_ex.txt"))
  (def example-1 (parse-rocks (first example)))
  (print (first example))
  (find-mirrors example-1)
  (not-mirror? (vals (:rows example-1)) (:dim-x example-1) 4)

  (final-calc 1 example)
  (final-calc 2 example)

  (def day 13)
  (def test-data (slurp (str "resources/day_" day ".txt")))

  (final-calc 1 test-data)
  (final-calc 2 test-data)
  (time (final-calc 1 test-data))
  (time (final-calc 2 test-data))

  )
