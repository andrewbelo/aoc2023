(ns day-1-trebuchet
  (:require
    [clojure.string :as str]))


(def test-data "resources/day_1_1.txt")


(defn digits-from-line
  [line]
  (filter #(<= (int \1) (int %) (int \9)) line))


(defn form-calibration-value
  [line]
  (let [digits (digits-from-line line)]
    (read-string (str (first digits) (last digits)))))


(defn get-all-calibration-values
  [text]
  (map form-calibration-value (str/split text #"\n")))


;; Part 1 result
(apply + (get-all-calibration-values (slurp test-data)))


;; Part 2
(defn first-digit-value
  [words-map line]
  (let [words (keys words-map)
        digits "123456789"
        words-re (re-pattern (str/join #"|" (into words digits)))
        found (re-find (re-matcher words-re line))]
    (str (words-map found found))))


(defn find-edge-digits
  [line]
  (let [worded-digits
        ["one" "two" "three" "four" "five" "six" "seven" "eight" "nine"]
        revered-worded-digits (map str/reverse worded-digits)
        form-meaning #(into {} (map vector % (range 1 10)))
        worded-digits-meaning (form-meaning worded-digits)
        reverse-worded-digits-meaning (form-meaning revered-worded-digits)
        find-left-fn (partial first-digit-value worded-digits-meaning)
        find-right-fn (partial first-digit-value reverse-worded-digits-meaning)]
    (read-string (str (find-left-fn line) (find-right-fn (str/reverse line))))))


(defn get-calibration-values-with-worded
  [text]
  (map find-edge-digits (str/split text #"\n")))


(def calibrate-values (get-calibration-values-with-worded (slurp test-data)))
(apply + calibrate-values)


(comment 
  ;; Testing bed 54649
  (def lines (str/split (slurp test-data) #"\n"))
  (def example 
"1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet")
  (def example-2
    "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen")
  (def line "ktvlhmq3xzmcztbplxlqzpqmoneightffd" )
  (def worded-digits
        ["one" "two" "three" "four" "five" "six" "seven" "eight" "nine"])

  (def meanings (into {} (map vector worded-digits (range 1 10))))
  (def matcher (re-matcher (re-pattern (str/join "|" worded-digits )) line))
  (re-find matcher)
  (int \9)
  (int \1)
  (str/split example #"\n")
  (str/replace-first "hellone413531one" "one" "1")
  (apply + (get-calibration-values-with-worded example-2))
  (find-edge-digits line)

  (apply + calibrate-values)

  (map #(list % (nth calibrate-values %) (nth lines %))
     (random-sample 0.01 (range 1 1000)))
  )
