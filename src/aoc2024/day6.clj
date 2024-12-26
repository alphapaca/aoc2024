(ns aoc2024.day6
  (:require [clojure.string :as str])
  (:require [clojure.core.match :refer [match]])
  (:require [clojure.set :refer [union]])
  (:require [clojure.tools.trace :as t]))

(def starting-direction [-1 0])

(def parse-cell
  {\. :empty
   \^ :visited
   \# :obstacle})

(defn parse-map [lines]
  (mapv (partial mapv parse-cell) lines))

(defn find-start-pos [lines]
  (->> lines
       (map-indexed (fn [x str] (when-let [y (str/index-of str \^)] [x y])))
       (some identity)))

(def next-direction
  {[1 0]  [0 -1]
   [0 1]  [1 0]
   [-1 0] [0 1]
   [0 -1] [-1 0]})

(defn count-visited [obstacle-map]
  (->> obstacle-map
       (mapcat identity)
       (filter (partial = :visited))
       (count)))

(defn find-positions-count [lines]
  (loop [direction    starting-direction
         obstacle-map (parse-map lines)
         pos          (find-start-pos lines)]
    (let [next-pos (mapv + pos direction)]
      (case (get-in obstacle-map next-pos)
            nil       (count-visited obstacle-map)
            :obstacle (recur (next-direction direction) obstacle-map pos)
            (recur direction (assoc-in obstacle-map next-pos :visited) next-pos)))))

(with-open [rdr (clojure.java.io/reader "resources/day6.in")]
  (find-positions-count (line-seq rdr)))

(def parse-cell-2
  {\. :empty
   \^ starting-direction
   \# :obstacle})

(defn parse-map-2 [lines]
  (mapv (partial mapv parse-cell-2) lines))

(defn check-cycle [start-direction start-obstacle-map start-pos]
  (loop [direction    start-direction
         obstacle-map start-obstacle-map
         pos          start-pos]
    (let [next-pos (mapv + pos direction)]
      (case (get-in obstacle-map next-pos)
            nil       false
            :obstacle (recur (next-direction direction) obstacle-map pos)
            :empty    (recur direction (assoc-in obstacle-map next-pos direction) next-pos)
            (if (= (get-in obstacle-map next-pos) direction)
              true
              (recur direction obstacle-map next-pos))))))

(defn find-cycles-count [lines]
  (loop [direction    starting-direction
         obstacle-map (parse-map lines)
         pos          (find-start-pos lines)
         cycles-count 0]
    (let [next-pos (mapv + pos direction)]
      (case (get-in obstacle-map next-pos)
            nil       cycles-count
            :obstacle (recur (next-direction direction) obstacle-map pos cycles-count)
            :empty    (recur direction
                        (assoc-in obstacle-map next-pos direction)
                        next-pos
                        (if (check-cycle (next-direction direction) (assoc-in obstacle-map next-pos :obstacle) pos)
                          (inc cycles-count)
                          cycles-count))
            ; if visited - don't change direction on obstacle map
            (recur direction obstacle-map next-pos cycles-count)))))

(time (with-open [rdr (clojure.java.io/reader "resources/day6.in")]
  (find-cycles-count (line-seq rdr))))
