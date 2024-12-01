(ns day-12-hot-springs
  (:require
    [clojure.edn :as edn]
    [clojure.set :as set]
    [clojure.string :as str]))


(defn zero-pad-right
  [s l]
  (concat s (repeat (- l (count s)) 0)))


(defn zero-pad-left
  [s l]
  (concat (repeat (- l (count s)) 0) s))


(defn line->spring
  [line]
  (let [[springs condition] (str/split line #" ")]
    [springs
     (mapv #(Integer/parseInt %) (str/split condition #","))]))


(defn line->spring-2
  [line]
  (let [[springs condition] (str/split line #" ")]
    [(str/join "?" (repeat 5 springs))
     (mapv #(Integer/parseInt %) (str/split (str/join "," (repeat 5 condition)) #","))]))


(defn trim-transform
  [springs]
  (->> springs
       (partition-by identity)
       (filter #(= \# (first %)))
       (map count)))


(defn trim-damaged
  [springs condition]
  (let [first-fixed (str/index-of springs \.)
        target-cond (first condition)]
    (if (and
          (>= (count springs) target-cond)
          (or
            (nil? first-fixed)
            (<= target-cond first-fixed))
          (not= \# (first (subs springs target-cond))))
      [(subs springs (min (count springs) (inc target-cond)))
       (drop 1 condition)]
      nil)))


(defn trim-knowns
  [springs condition]
  (case (first springs)
    \. [(str/replace springs #"^\.+" "") condition]
    \# (trim-damaged springs condition)
    \? [springs condition]))


(defn solve-all-known
  [springs condition]
  (if (= condition (trim-transform springs)) 1 0))


(defn propose-fixed-and-trim
  [springs condition]
  [(str/replace (subs springs 1) #"^\.+" "") condition])


(defn propose-damaged-and-trim
  [springs condition]
  (trim-damaged (str/join "" ["#" (subs springs 1)]) condition))


(def solve-line
  (memoize
    (fn [input]
      (if (nil? input) 0
          (let [[springs condition] input]
            (cond
              (and  (empty? condition) (nil? (str/index-of springs "#"))) 1
              (empty? condition) 0
              (empty? springs) 0

              (nil? (str/index-of springs "?"))
              (solve-all-known springs condition)

              (#{\# \.} (first springs))
              (solve-line (trim-knowns springs condition))

              (= \? (first springs))
              (+
                (solve-line (propose-damaged-and-trim springs condition))
                (solve-line (propose-fixed-and-trim  springs condition)))))))))


(defn final-calc
  [part text]
  (let [smth (case part
               1 ())]
    (->> text)))


(comment
  ;; Testing
  (def result  #(final-calc 1 %))
  (def result-2  #(final-calc 2 %))
  (def example
"???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1")

  (def example-2 
    "???#???.#??####? 5,1,5
???#????.#??????? 2,1,6
??.??#??#????.???. 1,10,1,1
#??#.???#..?? 4,1,1,1
?????.???..?? 1,2,2,1
????##?..??#?? 1,4,5
?#???.??##?#?. 3,1,3,1")

  (solve-line (line->spring "?###???????? 3,2,1"))
  (solve-line (line->spring ".??..??...?##. 1,1,3"))
  (solve-line (line->spring-2 ".??..??...?##. 1,1,3"))
  (apply + (map #(solve-line (line->spring %)) (str/split-lines example)))
  (apply + (map #(solve-line (line->spring-2 %)) (str/split-lines example)))

  (line->spring-2  "???.### 1,1,3")
  (str/join (repeat 5 "fajdkaf"))

  (map - [4 1 1] '(2))

  (result example)
  (result-2 example)

  (def day 12)
  (def test-data (slurp (str "resources/day_" day ".txt")))

  (def parsed-data (map line->spring (str/split-lines test-data)))
  (def parsed-data-2 (map line->spring-2 (str/split-lines test-data)))
  (count parsed-data)
  (apply + (map solve-line parsed-data))
  (apply + (map solve-line parsed-data-2))
  (result test-data)
  (result-2 test-data)
  (time (result test-data))
  (time (result-2 test-data))

  )
