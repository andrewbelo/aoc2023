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

(defn step
  [[s r] xmaps]
  (println s r xmaps)
  (or
    ;; full in
    (first (for [[d' s' r'] xmaps
                 :when (<= s' s (+ s r -1) (+ s' r' -1))
                 :let [_ (println s r "full in " d' s' r')]]
             [[(+ d' (- s s'))  r]]))

    ;; first half in
    (first (for [[d' s' r'] xmaps
                 :when (and  (<= s' s (+ s' r') (+ s r -1))
                             (not= s (+ s' r'))
                             (not= s (+ s' r')))
                 :let [part-in (- (+ s' r') s)
                       _ (println s r "right in " d' s' r' "(" d' (- s s') ")")]]
             (into
               [[(+ d' (- s s')) part-in]]
               (step [(+ s part-in) (- r part-in)] xmaps))))

    ;; second half in
    (first (for [[d' s' r'] xmaps
                 :when (and  (<= s s' (+ s r -1) (+ s' r' -1))
                             (not= s' (+ s r)))
                 :let [part-in (- (+ s r) s')
                       _ (println "left")]]
             (into
               [[d' part-in]]
               (step [s (- s' s)] xmaps))))

    ;; no in
    [[s r]]))


(defn full-translation
  [seeds xmaps-list]
  (reduce (fn [seeds-ranges xmaps] (mapcat #(step % xmaps) seeds-ranges))
          [seeds]
          xmaps-list))


(defn all-full-translation
  [[seeds-list xmaps-list]]
  (->>
    (mapcat #(full-translation % xmaps-list) seeds-list)
    (sort-by first)
    (first)))


(defn result-2
  [text]
  (->> text
       (almanac-2)
       (all-full-translation)))


(reduce)


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


  (def input (almanac-2 example))
  (def seeds-ranges (first input))
  (def xmaps-list (second input))


  (step [60 10] [[90 50 40]])
  (step [85 10] [[190 50 40] [10 93 20]])

  (reduce step [])
  (full-translation [79 14] xmaps-list)
  (all-full-translation [ [[79 14] [55 12]] xmaps-list])
  (result-2 example)

  (result example)
  (result-2 example)

  (def test-data (slurp (str "resources/day_" day ".txt")))
  (result test-data)
  (result-2 test-data) ; [77435348 18026321]
  (time (result-2 test-data))
  )
