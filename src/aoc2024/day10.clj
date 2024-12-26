(ns aoc2024.day10
  (:require [clojure.string :as str])
  (:require [clojure.core.match :refer [match]])
  (:require [clojure.set :refer [union]])
  (:require [clojure.tools.trace :as t]))

(defn parse-lines [lines]
  (mapv #(mapv (comp parse-long str) %) lines))

(defn get-all-prev-positions [hike-map x y h]
  (->> (list [(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)])
       (filter (fn [coord] (= (get-in hike-map coord) h)))))

(defn find-all-pos-of-0s-to-pos-of-9s [hike-map]
  (loop [trails-coords-to-pos-of-9s (->> hike-map
                                         (map-indexed (fn [x line] (map-indexed (fn [y h] [x y h]) line)))
                                         (mapcat identity)
                                         (filter (fn [[x y h]] (= h 9)))
                                         (map (fn [[x y _]] [[x y] #{[x y]}]))
                                         (into {}))
         next-h                     8]
    (if (< next-h 0)
      trails-coords-to-pos-of-9s
      (recur
        (->> trails-coords-to-pos-of-9s
             (mapcat
               (fn [[[x y] pos-of-9]]
                 (map #(vector % pos-of-9) (get-all-prev-positions hike-map x y next-h))))
             (group-by first)
             (map (fn [[coord scores]] [coord (->> scores (map second) (apply union))]))
             (into {}))
        (dec next-h)))))

(defn find-sum-of-trailhead-scores [lines]
  (->> (parse-lines lines)
       (find-all-pos-of-0s-to-pos-of-9s)
       (map second)
       (map count)
       (apply +)))

(time
 (with-open [rdr (clojure.java.io/reader "resources/day10.in")]
   (find-sum-of-trailhead-scores (line-seq rdr))))

(defn find-all-trailhead-ratings [hike-map]
  (loop [trails-coords-to-pos-of-9s (->> hike-map
                                         (map-indexed (fn [x line] (map-indexed (fn [y h] [x y h]) line)))
                                         (mapcat identity)
                                         (filter (fn [[x y h]] (= h 9)))
                                         (map (fn [[x y _]] [[x y] 1]))
                                         (into {}))
         next-h                     8]
    (if (< next-h 0)
      trails-coords-to-pos-of-9s
      (recur
        (->> trails-coords-to-pos-of-9s
             (mapcat
              (fn [[[x y] rating]]
                (map #(vector % rating) (get-all-prev-positions hike-map x y next-h))))
             (group-by first)
             (map (fn [[coord ratings]] [coord (->> ratings (map second) (apply +))]))
             (into {}))
        (dec next-h)))))

(defn find-sum-of-trailhead-ratings [lines]
  (->> (parse-lines lines)
       (find-all-trailhead-ratings)
       (map second)
       (apply +)))

(time
 (with-open [rdr (clojure.java.io/reader "resources/day10.in")]
   (find-sum-of-trailhead-ratings (line-seq rdr))))
