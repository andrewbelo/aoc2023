(ns day-6
  (:require
    [clojure.set :as set]
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
  (first (for [tt (range t)
               :let [dist (* tt (- t tt))]
               :when (> dist record)]
           [tt (- t tt)])))


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

  (race-document example)
  (race-document-2 example)

  (result example)
  (result-2 example)

  (def test-data (slurp (str "resources/day_" day ".txt")))
  (race-document-2 test-data)
  (result test-data) ; 1083852
  (result-2 test-data) ; 23501589
  (time (result test-data)) ; (out) "Elapsed time: 1.26423 msecs"
  (time (result-2 test-data)) ; (out) "Elapsed time: 514.84896 msecs"
  )
