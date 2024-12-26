(ns aoc2024.day1
  (:require [clojure.string :as str]))

(defn parse-input [lines]
  (->> lines
       (map #(str/split % #"\s+"))
       #_(map #(map parse-long %))
       (map (partial map parse-long))
       (apply map vector)))

(defn evaluate-list-distance [lines]
  (->> (parse-input lines)
       (map sort)
       (apply map vector)
       #_(map (fn [[left right]] (abs (- left right))))
       (map (comp abs (partial apply -)))
       (apply +)))

#_2196996
(with-open [rdr (clojure.java.io/reader "resources/day1.in")]
  (evaluate-list-distance (line-seq rdr)))

(defn evaluate-similarity-score [lines]
  (let [[left right] (parse-input lines)
        right-freqs  (frequencies right)]
    (->> left
         (map #(* % (or (right-freqs %) 0)))
         (apply +))))

#_23655822
(with-open [rdr (clojure.java.io/reader "resources/day1.in")]
  (evaluate-similarity-score (line-seq rdr)))
