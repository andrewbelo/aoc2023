(ns day-5
  (:require
    [clojure.set :as set]
    [clojure.string :as str]))


(def day 5)


(defn splitter
  [sep line]
  (str/split line sep))


;; Parsing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn seeds
  [line]
  (->> line
       (splitter #":")
       (second)
       (str/trim)
       (splitter #" ")
       (map read-string)))


(defn single-map
  [map-lines]
  (let [[_ & ranges]  (str/split-lines map-lines)
        read-ranges       (map (partial splitter #" ") ranges)]
    (into [] (map #(map read-string %) read-ranges))))


(defn almanac
  [text]
  (let [[seeds-line & maps] (str/split text #"\n\n")]
    [(seeds seeds-line) (map single-map maps)]))


(defn almanac-2
  [text]
  (let [[seeds-line & maps] (str/split text #"\n\n")]
    [(partition 2 (seeds seeds-line))
     (map single-map maps)]))


;; Part 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn match-seed-to-map
  [dst-maps seed]
  (or (first (for [[dst src r] dst-maps
                   :when (<= src seed (+ r src -1))]
               (+ seed (- dst src))))
      seed))


(defn apply-map
  [seeds-map dst-maps]
  (map #(match-seed-to-map dst-maps %) seeds-map))


(defn read-almanac
  [[seeds-set dst-maps]]
  (reduce #(apply-map %1 %2) seeds-set dst-maps))


(defn result
  [text]
  (->> text
       (almanac)
       (read-almanac)
       (apply min)))


;; Part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn match-seed-to-map-2
  [dst-maps [s len]]
  (or (first (for [[dst src r] dst-maps
                   :let [step (- s src)]
                   :when (<= src s  (+ s len) (+ r src -1))]
               [(+ dst step) (+ dst step len)]))
      (first (for [[dst src r] dst-maps
                   :let [step (- s src)]
                   :when (<= src s  (+ s len) (+ r src -1))]
               [(+ dst step) (+ dst step len)]))
      [s len]))


(defn apply-map-2
  [seeds-ranges dst-maps]
  (mapcat #(match-seed-to-map-2 dst-maps %) seeds-ranges))


(defn read-almanac-2
  [[seeds-ranges levels]]
  (reduce #(apply-map-2 %1 %2) seeds-ranges levels))


(defn result-2
  [text]
  (->> text
       (almanac-2)
       (read-almanac-2)
       (apply min)))


(map #(+ 4 %) #{1 5 13 3 25})


(comment
  ;; Testing
  (def example (slurp "resources/day_5_ex.txt") )
  (def seeds-map (first (almanac example )))
  (def maps (second (almanac example)))
  (single-map (second example))
  (apply-map seeds-map (first maps))
  ;; (57 13 14 81)
  (match-seed-to-map [[ 0 15 37 ] [ 37 52 2 ] [ 39 0 15 ]] 13)
  (match-seed-to-map (first maps) 13)
  (read-almanac (almanac example))


  (almanac-2 example)
  (result example)
  (result-2 example)

  (def test-data (slurp (str "resources/day_" day ".txt")))
  (result test-data)
  (result-2 test-data)
  )
