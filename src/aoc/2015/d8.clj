(ns y-2015.d8
  (:require
    [clojure.set :as set]
    [clojure.string :as str]))


(defn santa-str-count
  [line]
  (count (first (reduce (fn [[groups state] c]
                          (let [state'  (case  state
                                          :x-2 :new
                                          :x   :x-2
                                          :new (if (= c \\) :esc :new)
                                          :esc (if (= c \x) :x :new))
                                groups' (if (= state :new)
                                          (conj groups [c])
                                          (conj (drop 1  groups)
                                                (conj (first groups) c)))]
                            [groups' state']))  ['() :new] line))))


(defn count-diff
  [line]
  (- (count line) (santa-str-count (drop 1 (drop-last line)))))


(defn count-diff-2
  [line]
  (- (apply + 2 (map #(if (#{\\ \"} %) 2 1) line)) (count line)))


(apply + 2  (map #(if (#{\\ \"} %) 2 1) "\"aaa\\\"aaa\""))
(apply + 2  (map #(if (#{\\ \"} %) 2 1) "\"\\x27\""))

(santa-str-count "aaa\\\"aaa")
(count-diff "\"aaa\\\"aaa\"")
(count "aaa\\\"aaa")


(comment 
  ;; Rich comment
  (def day 8)
  (def test-file (str "resources/2015/" day))
  (count  "aaa\"aaa")
(subs  "v\xfb\"lgs\"kvjfywmut\x9cr" 1 -1)
  (subs  "fhdk" 1 -1)

  
  (def test (str/split-lines (slurp test-file)))
  (apply + (map count-diff test))
  (apply + (map count-diff-2 test))

  (partition 4 1 test)

  (santa-str-count test)
  ()
  (conj [1 2 3] 4)
"azlgxdbljwygyttzkfwuxv"
"v\xfb\"lgs\"kvjfywmut\x9cr"
"merxdhj"
"dwz"
"d\\gkbqo\\fwukyxab\"u"
"k\xd4cfixejvkicryipucwurq\x7eq"
"nvtidemacj\"hppfopvpr"
"kbngyfvvsdismznhar\\p\"\"gpryt\"jaeh"
"khre\"o\x0elqfrbktzn"
  (into [] "fhgk\x")
  )
