(ns aoc2024.day8
  (:require [clojure.string :as str])
  (:require [clojure.core.match :refer [match]])
  (:require [clojure.set :refer [union]])
  (:require [clojure.tools.trace :as t]))

(defn find-unordered-pairs [coll]
  (loop [[head & tail] coll
         result        '()]
    (if (nil? head)
      result
      (recur tail (apply conj result (map #(vector head %) tail))))))

(defn find-node-type-to-coords-list [lines]
  (->>
   (map-indexed (fn [x line] (map-indexed (fn [y node] [x y node]) line)) lines)
   (mapcat identity)
   (filter (fn [[_ _ node]] (not= node \.)))
   (group-by (fn [[_ _ node]] node))
   (map
    (fn [[node coords-with-node]] [node (mapv (fn [[x y _]] [x y]) coords-with-node)]))
   (into {})))

(defn find-antinodes [[[x1 y1] [x2 y2]]]
  (let [dx (- x2 x1)
        dy (- y2 y1)]
    [[(- x1 dx) (- y1 dy)] [(+ x2 dx) (+ y2 dy)]]))

(defn find-antinodes-coords [node-coords]
  (->> (find-unordered-pairs node-coords)
       (mapcat find-antinodes)))

(defn find-antinodes-count [lines]
  (->> (find-node-type-to-coords-list lines)
       (mapcat (fn [[node coords]] (find-antinodes-coords coords)))
       (filter (fn [[x y]] (and (< -1 x (count lines)) (< -1 y (count (first lines))))))
       (into #{})
       (count)))

(time
 (with-open [rdr (clojure.java.io/reader "resources/day8.in")]
   (find-antinodes-count (line-seq rdr))))

(defn find-antinodes-2 [[x1 y1] [x2 y2] [max-x max-y]]
  (let [dx (- x2 x1)
        dy (- y2 y1)]
    (concat
      (->> (iterate (fn [[xi yi]] [(- xi dx) (- yi dy)]) [x1 y1])
           (take-while (fn [[x y]] (and (< -1 x max-x) (< -1 y max-y))))
           #_(drop 1))
      (->> (iterate (fn [[xi yi]] [(+ xi dx) (+ yi dy)]) [x2 y2])
           (take-while (fn [[x y]] (and (< -1 x max-x) (< -1 y max-y))))
           #_(drop 1)))))

(defn find-antinodes-coords-2 [node-coords upper-bounds]
  (->> (find-unordered-pairs node-coords)
       (mapcat (fn [[coord1 coord2]] (find-antinodes-2 coord1 coord2 upper-bounds)))))

(defn print-map [max-x max-y coords]
  (->> (range max-x)
       (map (fn [x] (map (fn [y] (if (coords [x y]) \# \.)) (range max-y))))
       (map #(apply str %))
       (map println)))

(defn find-antinodes-count-2 [lines]
  (->> (find-node-type-to-coords-list lines)
       (mapcat
        (fn [[node coords]]
          (find-antinodes-coords-2 coords [(count lines) (count (first lines))])))
       (into #{})
       #_(print-map (count lines) (count (first lines)))
       (count)))

(time
 (with-open [rdr (clojure.java.io/reader "resources/day8.in")]
   (find-antinodes-count-2 (line-seq rdr))))

; ##....#....#
; .#.#....0...
; ..#.#0....#.
; ..##...0....
; ....0....#..
; .#...#A....#
; ...#..#.....
; #....#.#....
; ..#.....A...
; ....#....A..
; .#........#.
; ...#......##