(ns y-2015.d7
  (:require
    [clojure.set :as set]
    [clojure.string :as str]))


(defn unsigned-16-bit
  [n]
  (bit-and n 0xFFFF))


(defn str->wire-or-signal
  [s]
  (if (re-matches #"^[a-z]+$" s) (keyword s) (read-string s)))


(re-matches #"^[a-z]+$" "0")
(read-string "0")
(str->wire-or-signal "0")


(defn lhs->op
  ([wire-or-signal] [(str->wire-or-signal wire-or-signal)])
  ([_ wire-or-signal] [bit-not (str->wire-or-signal wire-or-signal)])
  ([wos-lhs op wos-rhs]
   (let [ops {"AND" bit-and
              "OR"  bit-or
              "LSHIFT" bit-shift-left
              "RSHIFT" bit-shift-right}]
     [(ops op)
      (str->wire-or-signal wos-lhs)
      (str->wire-or-signal wos-rhs)])))


(defn line->
  [line]
  (let [[lhs rhs] (str/split line #" -> ")]
    [(keyword rhs) (apply lhs->op (str/split lhs #" "))]))


(def calc
  (memoize
    (fn
      ([cmds wos]
       (println wos)
       (if (number? wos)
         wos
         (apply calc cmds (cmds wos))))
      ([cmds op wos]
       (unsigned-16-bit (op  (if (number? wos) wos
                                 (apply calc cmds (cmds wos))))))
      ([cmds op wos-lhs wos-rhs]
       (let [lhs (if (number? wos-lhs) wos-lhs
                     (apply calc cmds (cmds wos-lhs)))
             rhs  (if (number? wos-rhs) wos-rhs
                      (apply calc cmds (cmds wos-rhs)))]
         (unsigned-16-bit (op lhs rhs)))))))


(comment 
  ;; Rich comment
  (def day 7)
  (def test-file (str "resources/2015/" day))
  (def example 
"123 -> x
456 -> y
x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> i"
    )


  (line-> "x OR y -> e")
  (line-> "456 -> y")
  (line-> "y RSHIFT 2 -> g")
  (def cmds (into {} (map line-> (str/split-lines example))))
  (def test-cmds (into {} (map line-> (str/split-lines (slurp test-file)))))


  (unsigned-16-bit (bit-not 123))
  (calc cmds :x)
  (calc cmds :h)
  (calc cmds :f)
  (calc cmds :h)

  (calc test-cmds :a)
  (calc (assoc test-cmds :b [46065]) :a)
  (cmds :x)

  )
