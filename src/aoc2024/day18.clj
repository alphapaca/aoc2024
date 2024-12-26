(ns aoc2024.day18
  (:require [clojure.string :as str])
  (:require [clojure.core.match :refer [match]])
  (:require [clojure.math :refer [log10 pow floor]])
  (:require [clojure.set :refer [union]])
  (:require [clojure.math.combinatorics :refer [cartesian-product]])
  (:require [clojure.tools.trace :as t])
  (:require [loom.alg :as alg])
  (:require [loom.graph :as graph]))

(defn parse-lines [lines]
  (->> (map #(str/split % #",") lines)
       (mapv #(mapv parse-long %))))

(defn assoc-multimap [multimap key elem]
  (let [bucket (get multimap key [])]
    (assoc multimap key (conj bucket elem))))

(defn dissoc-first [multimap]
  (let [[key values] (first multimap)]
    (if (= (count values) 1)
      (dissoc multimap key)
      (assoc multimap key (subvec values 1)))))

(defn dissoc-value [multimap key value]
  (let [filtered (filterv #(not= value %) (multimap key))]
    (if (empty? filtered)
      (dissoc multimap key)
      (assoc multimap key filtered))))

(defn dijkstra [start-v end-v edges]
  (loop [scores-to-vertices (sorted-map 0 [start-v])
         vertices-to-score  {start-v 0}
         visited            #{}]
    (let [[[score [vertice]] & _] scores-to-vertices
          next-vertices           (->> (edges vertice)
                                       (map
                                        (fn [[v edge-s]]
                                          (let [new-s (+ score edge-s)
                                                old-s (get vertices-to-score v)]
                                            (when (and (not (visited v)) (or (nil? old-s) (<= new-s old-s)))
                                                  [v new-s old-s]))))
                                       (filter some?))]
      (cond
       (nil? vertice)    (do (println scores-to-vertices vertices-to-score visited) nil)
       (= vertice end-v) score
       :else             (recur
                           (reduce
                            (fn [acc [v s old-s]]
                              (-> (if (some? old-s) (dissoc-value acc old-s v) acc)
                                  (assoc-multimap s v)))
                            (dissoc-first scores-to-vertices)
                            next-vertices)
                           (reduce (fn [acc [v s _]] (assoc acc v s)) vertices-to-score next-vertices)
                           (conj visited vertice))))))

(defn do-task [lines [size-x size-y] limit]
  (let [walls (->> (parse-lines lines)
                   (take limit)
                   (into #{}))]
    (->> (range size-y)
         (mapcat (fn [y] (map #(vector % y) (range size-x))))
         (remove walls)
         (map
          (fn [[x y :as v]]
            (->> (list [(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)])
                 (filter
                  (fn [[xi yi :as vi]]
                    (and (>= xi 0) (>= yi 0) (< xi size-x) (< yi size-x) (not (walls vi)))))
                 (map #(vector % 1))
                 (into {})
                 (vector v))))
         (into {})
         (dijkstra [0 0] [(dec size-x) (dec size-y)])
         #_(count))))

(time
 (with-open [rdr (clojure.java.io/reader "resources/day18_test.in")]
   (do-task (line-seq rdr) [7 7] 12)))

(time
 (with-open [rdr (clojure.java.io/reader "resources/day18.in")]
   (do-task (line-seq rdr) [71 71] 1024)))

(defn eval-start-graph [walls-list [size-x size-y] limit]
  (let [walls (->> walls-list
                   (take limit)
                   (into #{}))]
    (->> (range size-y)
         (mapcat (fn [y] (map #(vector % y) (range size-x))))
         (remove walls)
         (map
          (fn [[x y :as v]]
            (->> (list [(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)])
                 (filter
                  (fn [[xi yi :as vi]]
                    (and (>= xi 0) (>= yi 0) (< xi size-x) (< yi size-x) (not (walls vi)))))
                 (map #(vector % 1))
                 (into {})
                 (vector v))))
         (into {}))))

(defn do-task-2 [lines [size-x size-y :as size] start-limit]
  (let [walls-list (parse-lines lines)]
    (loop [graph      (eval-start-graph walls-list size start-limit)
           idx-to-add start-limit]
      (let [new-wall             (walls-list idx-to-add)
            adjacent-to-new-wall (graph new-wall)
            new-graph            (reduce
                                  (fn [acc v]
                                    (if (= 1 (count (acc v)))
                                      (dissoc acc v)
                                      (assoc acc v (dissoc (acc v) new-wall))))
                                  (dissoc graph new-wall)
                                  adjacent-to-new-wall)]
        (if (nil? (dijkstra [0 0] [(dec size-x) (dec size-y)] new-graph))
          new-wall
          (recur new-graph (inc idx-to-add)))))))

(time
 (with-open [rdr (clojure.java.io/reader "resources/day18_test.in")]
   (do-task-2 (line-seq rdr) [7 7] 12)))

(time
 (with-open [rdr (clojure.java.io/reader "resources/day18.in")]
   (do-task-2 (line-seq rdr) [71 71] 1024)))