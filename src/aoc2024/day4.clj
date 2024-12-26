(ns aoc2024.day4
  (:require [clojure.string :as str])
  (:require [clojure.core.match :refer [match]]))

(def directions [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]])

(defn have-xmas-in-direction? [data x y [direction-x direction-y]]
  (->>
   (map vector "XMAS" (iterate (partial + direction-x) x) (iterate (partial + direction-y) y))
   (every? (fn [[char xi yi]] (= (get-in data [xi yi]) char)))))

(defn count-xmas-for-coord [data x y]
  (if (not= (get-in data [x y]) \X)
    0
    (->> directions
         (filter (partial have-xmas-in-direction? data x y))
         (count))))

(defn find-all-xmas [lines]
  (let [data (vec lines)]
    (->> (range (count data))
         (mapcat #(map (partial vector %) (range (count (get data %)))))
         (map (fn [[x y]] (count-xmas-for-coord data x y)))
         (apply +))))

(with-open [rdr (clojure.java.io/reader "resources/day4.in")]
  (find-all-xmas (line-seq rdr)))

(defn get-coord-with-neighbours [data x y]
  (let [xs [(dec x) x (inc x)]
        ys [(dec y) y (inc y)]]
    (mapv (fn [xi] (mapv #(get-in data [xi %]) ys)) xs)))

(defn have-x-mas? [data]
  (match data
         [[\M _ \M]
          [_ \A _]
          [\S _ \S]] true

         [[\M _ \S]
          [_ \A _]
          [\M _ \S]] true

         [[\S _ \S]
          [_ \A _]
          [\M _ \M]] true

         [[\S _ \M]
          [_ \A _]
          [\S _ \M]] true

         :else false))

(defn have-x-mas-in-coord? [data x y]
  (have-x-mas? (get-coord-with-neighbours data x y)))

(defn find-all-x-mas [lines]
  (let [data (vec lines)]
    (->> (range (count data))
         (mapcat #(map (partial vector %) (range (count (get data %)))))
         (filter (fn [[x y]] (have-x-mas-in-coord? data x y)))
         (count))))

(with-open [rdr (clojure.java.io/reader "resources/day4.in")]
  (find-all-x-mas (line-seq rdr)))