(ns y-2023.d20
  (:require
    [clojure.set :as set]
    [clojure.string :as str]))


(defn line->modules
  [line]
  (let [[lhs rhs]       (str/split line #" -> ")
        [mtype module]  (if (= lhs "broadcaster")
                          [:broadcaster :broadcaster]
                          [(keyword  (subs lhs 0 1)) (keyword (subs lhs 1))])
        dst             (mapv keyword (str/split rhs #", "))]
    [module [mtype dst []]]))


(defn distribute-srcs
  [system  [module [_ dsts _]]]
  (reduce
    (fn [sys dst]
      (println sys dst)
      (let [dst-module (sys dst)
            dst-srcs   (conj (last dst-module) module)
            module'     (assoc dst-module 2 dst-srcs)]
        (assoc sys dst module')))
    system
    dsts))


(defn text->system
  [text]
  (let [modules (into {} (map line->modules (str/split-lines text)))]
    (reduce distribute-srcs modules modules)))


(comment
  (def day 20)
  (def test-data (slurp (str  "resources/2023/" day)))
  (def ex1-data (slurp (str "resources/2023/" day "e1")))
  (def ex2-data (slurp (str  "resources/2023/" day "e2")))


  (into {} (map line->modules (str/split-lines ex1-data)))
  (text->system ex1-data)

  )

