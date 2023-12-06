(ns day-6-wait-for-it
  (:require
    [clojure.string :as str]))


(def day 6)


;; Parsing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn splitter
  [sep]
  (fn [text] (str/split text sep)))


(defn split-race-document
  [text extra-steps]
  (map (comp extra-steps
             (splitter #" +")
             second
             (splitter #": +"))
       (str/split-lines text)))


(defn race-document
  [text]
  (split-race-document text #(map read-string %)))


(defn race-document-2
  [text]
  (split-race-document text (comp read-string #(apply str %))))


;; Part 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def range-len (fn [[l r]] (inc (- r l))))


(defn error-margin
  [[t record]]
  (let [d       (Math/sqrt (- (* t t) (* 4 record)))
        [x1 x2] [(int (Math/ceil  (/ (- t d) 2)))
                 (int (Math/floor (/ (+ t d) 2)))]
        check   #(- (* % (- t %)) record)
        adjust  (fn [x move] (if-not (zero? (check x)) x (move x)))]
    (mapv adjust [x1 x2] [inc dec])))


(defn result
  [text]
  (->> text
       (race-document)
       (apply map vector)
       (map error-margin)
       (map range-len)
       (apply *)))


;; Part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn result-2
  [text]
  (->> text
       (race-document-2)
       (error-margin)
       (range-len)))


(comment
  ;; Testing
  (def example
"Time:      7  15   30
Distance:  9  40  200")

  (error-margin [7 9])
  (map error-margin (apply map vector (race-document example)))
  (race-document-2 example)

  (result example)
  (result-2 example)

  (def test-data (slurp (str "resources/day_" day ".txt")))
  (race-document-2 test-data)
  (result test-data)
  (result-2 test-data)
  )
