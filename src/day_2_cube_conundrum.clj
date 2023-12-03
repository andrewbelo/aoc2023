(ns day-2-cube-conundrum
  (:require
    [clojure.string :as str]))


(defn remove-prefix
  [line]
  (let [semi-id (str/index-of line ":")]
    (subs line (+ 2 semi-id))))


(defn spliting-factory
  [re s]
  (map str/trim (str/split s re)))


(defn parse-move-part
  [move-part]
  (let [[ammout color] (str/split move-part #" ")]
    [(keyword color) (read-string ammout)]))


(defn parse-move
  [move]
  (let [move-parts (spliting-factory #"," move)]
    (into {} (map parse-move-part move-parts))))


(defn parse-game
  [game]
  (let [moves (spliting-factory #";" game)
        mapped-moves (map parse-move moves)]
    (apply merge-with max mapped-moves)))


(defn parse-games
  [text]
  (->> (str/split-lines text)
       (map remove-prefix)
       (map parse-game)))


(defn possible-game?
  [game]
  (let [game-restrictions {:red 12, :green 13, :blue 14}
        check (merge-with - game-restrictions game)]
    (empty? (filter #(< (second %) 0) check))))


(defn result
  [text]
  (let [checks    (map possible-game? (parse-games text))
        with-ids  (map #(vector (inc %1) %2) (range) checks)]
    (println with-ids)
    (apply + (map first (filter #(second %) with-ids)))))


;; Part 1 result
(def test-data (slurp "resources/day_2.txt"))
(result test-data)


;; Part 2 result
(defn cube-power
  [cubes]
  (apply * (map second cubes)))


(defn result2
  [text]
  (let [min-needed  (parse-games text)
        cube-powers (map cube-power min-needed)]
    (apply + cube-powers)))


(result2 test-data)


(comment 
  ;; Testings part 1
  (def example
"Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

  (def game "6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green" )
  (parse-game (spliting-factory #";" game))
  (map remove-prefix(str/split-lines example))
  (parse-games example)
  (possible-game? {:green 26, :blue 11, :red 25})
  (cube-power {:green 14, :blue 3, :red 15})
  ( (map possible-game? (parse-games example)))
  (result example)
  (result2 example)
  )
