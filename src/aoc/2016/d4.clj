(ns y-2016.d4
  (:require
    [clojure.string :as str]))


(int \a) ; 97
(int \z) ; 122


(defn decrypt-word
  [w mov]
  (apply str (for [c w
                   :let [d  (mod mov 26)
                         c' (+ (int c) d)
                         o  (if (> c' (int \z))
                              (+ (int \a) (- c' (int \z) 1))
                              c')]]
               (if (<= (int \a) (int c) (int \z))  (char o) c))))


(int \e)
(mod -13 10)


(defn parse-rooms
  [line]
  (let [[room check] (str/split line #"\[")
        names (str/split room #"-")
        room-words (->> names
                        (drop-last)
                        (apply str)
                        (frequencies)
                        (sort-by (juxt #(- (second %)) first))
                        (take 5)
                        (map first)
                        (apply str))
        check       (subs check 0 (dec (count check)))
        sector-id   (Integer/parseInt (last names))
        decrypted   (decrypt-word (str/join " " (drop-last names))
                                  sector-id)]
    (if (= room-words check) [decrypted sector-id] nil)))


(subs "123456" 0 (dec  (count "123456")))


(comment 
  ;; Rich comment
  (def day 4)
  (str "https://adventofcode.com/2016/day/" day) ; "https://adventofcode.com/2016/day/4"
  (def test-file (str "resources/2016/" day))


; aaaaa-bbb-z-y-x-123[abxyz] is a real room because the most common letters are 
; a (5), b (3), and then a tie between x, y, and z, which are listed alphabetically.
; a-b-c-d-e-f-g-h-987[abcde] is a real room because although the letters are all
; tied (1 of each), the first five are listed alphabetically.
; not-a-real-room-404[oarel] is a real room.
; totally-real-room-200[decoy] is not.

  (def lines (str/split-lines (slurp test-file)))
  (parse-rooms (first lines))
  (def rooms (apply + (map parse-rooms lines))) ; #'y-2016.d4/rooms
; 361724
  (->> rooms

       )
  (filter #(str/index-of (first %) "storage") (filter some? (map parse-rooms lines)))

  )
