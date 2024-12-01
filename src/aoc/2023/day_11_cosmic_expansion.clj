(ns day-11-cosmic-expansion
  (:require
   [clojure.set :as set]
   [clojure.string :as str]))

(defn galaxies
  [sky]
  (for [y (range (count sky))
        x (range (count (first sky)))
        :when (= "#" (get-in sky [y x]))]
    {:x x, :y y}))

(defn spaces
  [sky gs]
  (let [[xs ys] ((juxt #(map :x %) #(map :y %)) gs)
        cols (set (range (count (first sky))))
        rows (set (range (count sky)))]
    {:col (set/difference cols (set xs))
     :row (set/difference rows (set ys))}))

(defn distance
  [g-lhs g-rhs]
  (let [[xs ys] ((juxt #(map :x %) #(map :y %)) [g-lhs g-rhs])]
    (+ (Math/abs (apply - xs)) (Math/abs (apply - ys)))))

(defn find-nearest
  ([gs] (into {} (for [g1 gs, g2 gs :when (not= g1 g2)]
                   {[g1 g2] (distance g1 g2)}))))

(defn expand-universe
  [galaxs space]
  (for [galaxy galaxs
        :let [[x y]     ((juxt :x :y) galaxy)
              cols-move (count (filter #(< % x) (:col space)))
              rows-move (count (filter #(< % y) (:row space)))]]
    {:x (+ x cols-move)
     :y (+ y rows-move)}))

(defn expand-universe-2
  [galaxs space]
  (for [galaxy galaxs
        :let [[x y]     ((juxt :x :y) galaxy)
              cols-move (count (filter #(< % x) (:col space)))
              rows-move (count (filter #(< % y) (:row space)))]]
    {:x (+ x (* 999999 cols-move))
     :y (+ y (* 999999 rows-move))}))

(defn final-calc
  [part text]
  (let [expander  (case part
                    1 expand-universe
                    2 expand-universe-2)
        sky   (mapv #(str/split % #"") (str/split-lines text))
        gs    (galaxies sky)
        space (spaces sky gs)
        neigs (find-nearest (expander gs space))]
    (/ (apply + (vals neigs)) 2)))

(comment
  ;; Testing
  (def example
    "...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....")
  (def example-sky (mapv #(str/split % #"") (str/split-lines example)))
  (def gs (galaxies example-sky))
  (def sp (spaces example-sky gs))
  (def expanded-universe (expand-universe gs sp))
  (find-nearest expanded-universe)
  (/ (apply + (vals (find-nearest expanded-universe))) 2)

  (final-calc 1 example)
  (final-calc 2 example)

  (def day 11)
  (def test-data (slurp (str "resources/day_" day ".txt")))

  (final-calc 1 test-data)
  (final-calc 2 test-data)
  (time (final-calc 1 test-data))
  (time (final-calc 2 test-data)))
