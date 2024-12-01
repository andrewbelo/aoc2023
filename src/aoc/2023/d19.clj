(ns y-2023.d19
  (:require
    [clojure.pprint :as pp]
    [clojure.set :as set]
    [clojure.string :as str]))


(def WF-DIV #"\{") ; }


(defn text->rule
  ([restriction dst]
   (let [[src n]  (str/split restriction #"[><]")
         op       (case  (nth restriction (count src))
                    \> >
                    \< <)]
     [(keyword src) [op  (read-string n)]  (keyword dst)]))
  ([text]
   (let [parts (str/split text #":")]
     (if (= 1 (count parts)) (map keyword parts) (apply text->rule parts)))))


(defn text->workflows
  [line]
  (let [[n rules-raw] (str/split (subs line 0 (dec (count line))) WF-DIV)
        rules   (map text->rule (str/split rules-raw #","))]
    [(keyword n) rules]))


(defn text->parts
  [line]
  (let [cats (str/split (subs line 1 (dec (count line))) #",")
        nums (mapv (comp read-string second #(str/split % #"=")) cats)]
    (into {} (mapv vector [:x :m :a :s] nums))))


(defn parse-text
  [text]
  (let [[raw-workflows raw-parts] (str/split text #"\n\n")
        workflows (map text->workflows (str/split-lines raw-workflows))
        parts     (map text->parts (str/split-lines raw-parts))]
    [(into {} workflows) parts]))


(defn pass-workflow
  [part workflow]
  (reduce
    (fn [old rule]
      (if (= 1 (count rule))
        (reduced  (first rule))
        (let [[src [op thresh] dst] rule]
          (if (op (part src) thresh) (reduced dst) old))))
    nil
    workflow))


;; Part 2

(defn break-zone
  ([[zone _] src [op thresh] dst]
   (let [[from to]   (zone src)
         [in out]    (if (= op <)
                       [[from (dec thresh)] [thresh to]]
                       [[(inc thresh)  to] [from thresh]])]

     (if (<= from thresh to)
       [[(assoc zone src in) dst] [(assoc zone src out) nil]]
       (if (op from to thresh) [[zone dst] nil] [nil [zone nil]]))))
  ([[zone _] rule] [[zone rule]]))


(defn pass-workflow-with-zone
  [[zone dst] workflows]
  (reduce
    (fn [[curr-zone results] rule]
      (let [[in out]  (apply break-zone curr-zone rule)
            next-step [out (conj results in)]]
        (if (nil? out) (reduced next-step) next-step)))
    [[zone dst] []]
    (workflows dst)))


(defn finished?
  [[_ dst]]
  (boolean (#{:A :R} dst)))


(defn pass-workflows
  [workflows]
  (reduce
    (fn [[q finished] _]
      (let [r (mapcat #(second (pass-workflow-with-zone % workflows)) q)
            [finished' q'] (mapv (group-by finished? r) [true false])
            next-step [q' (into finished finished')]]
        (if (empty? q') (reduced next-step) next-step)))
    [[[{:x [1 4000]
        :m [1 4000]
        :a [1 4000]
        :s [1 4000]} :in]] []]
    workflows))


(defn accepted?
  [part workflows]
  (reduce (fn [next-workflow i]
            (let [res (pass-workflow part (workflows next-workflow))]
              (case  res
                :A (reduced true)
                :R (reduced false)
                res)))
          :in
          (range (count  workflows))))


(defn res
  [workflows parts]
  (filter #(accepted? % workflows) parts))


(comment
  (def day 19)
  (def test-input (slurp (str "resources/2023/" day)))
  (def example-input (slurp (str "resources/2023/" day "e")))

  (text->rule "s>3448:A")
  (text->parts "{x=2127,m=1623,a=2188,s=1013}") 

  (def ress (apply res (parse-text example-input)))
  (def ress (apply res (parse-text test-input)))
  (apply + (map #(apply + (map second %)) ress))
  (let [workflow '([:s #(< % 1351) :px] (:qqz))
        part {:x 787, :m 2655, :a 1222, :s 2876}
        ]
    (pass-workflow part workflow))
  (def workflows (first (parse-text example-input)))
  (def workflows (first (parse-text test-input)))

  (def ex-res (pass-workflows workflows))
  (defn zone-size [[zone _]]
    (apply * (mapv (fn [[ _ [a b]]] (inc (- b a ))) zone)))
  (apply + (map zone-size (filter #(= :A (second %)) (second ex-res))))
  (pass-workflow-with-zone [{:x [1 4000]
                             :m [1 4000]
                             :a [1 4000]
                             :s [1 4000]} :in] workflows)
 )
