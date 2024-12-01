(ns day-15
  (:require
    [clojure.string :as str]))


(defn elve-hash-fn
  [res new]
  (rem (* 17 (rem (+ res (int new)) 256)) 256))


(defn dash-cmd
  [box target-label]
  (apply vector (remove #(= target-label (first %)) box)))


(defn equa-cmd
  [box [label lense]]
  (let [lense-num (read-string lense)]
    (if-let [label-idx (first (keep-indexed #(when (= label (first %2)) %1) box))]
      (assoc box label-idx [label lense-num])
      (conj box [label lense-num]))))


(defn label
  [labels cmd]
  (let [cmd-parts   (str/split cmd #"[=\-]")
        label-hash  (reduce elve-hash-fn 0 (first cmd-parts))
        curr-box    (get labels label-hash [])
        cmd-result  (case (count cmd-parts)
                      1 (dash-cmd curr-box (first cmd-parts))
                      2 (equa-cmd curr-box  cmd-parts))]
    (assoc labels label-hash cmd-result)))


(defn focusing-power
  [boxes]
  (for [[box-num slots] boxes
        :let [slots-muls (map-indexed (fn [i s] (* (inc i) (second s))) slots)]]
    (apply + (map #(* (inc box-num) %) slots-muls))))


(defn final-calc
  [part text]
  (let [hashables (str/split (str/trim text) #",")
        hashes  (map #(reduce elve-hash-fn 0 %) hashables)]
    (case part
      1 (apply + hashes)
      2 (apply + (focusing-power (reduce label {} hashables))))))


(comment
  ;; Testing
  (def example "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")
  (str/split example #"[\,=\-1-9]+")

  (str/split example #",")

  (final-calc 1 example)
  (focusing-power(final-calc 2 example) )


  (def test-data (slurp (str "resources/day_" 15 ".txt")))
  (final-calc 1 test-data) ; 510388
  (final-calc 2 test-data) ; 291774
  (time (final-calc 1 test-data)) ; (out) "Elapsed time: 4.525002 msecs"
  (time (final-calc 2 test-data)) ; (out) "Elapsed time: 18.92061 msecs"
  )
