(ns aoc2024.day20
  (:require [clojure.string :as str])
  (:require [clojure.core.match :refer [match]])
  (:require [clojure.math :refer [log10 pow floor]])
  (:require [clojure.set :refer [union]])
  (:require [clojure.math.combinatorics :refer [cartesian-product]])
  (:require [clojure.tools.trace :as t]))

(def map-cells
  {\# :wall
   \. :empty
   \S :empty
   \E :empty})

(defn parse-lines [lines]
  [(->> lines
        (map-indexed (fn [x str] (when-let [y (str/index-of str \S)] [x y])))
        (some identity))
   (->> lines
        (map-indexed (fn [x str] (when-let [y (str/index-of str \E)] [x y])))
        (some identity))
   (mapv (fn [line] (mapv map-cells line)) lines)])

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

(def dijkstra
  (memoize
   (fn [start-v end-v edges]
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
          (nil? vertice)    nil
          (= vertice end-v) score
          :else             (recur
                              (reduce
                               (fn [acc [v s old-s]]
                                 (-> (if (some? old-s) (dissoc-value acc old-s v) acc)
                                     (assoc-multimap s v)))
                               (dissoc-first scores-to-vertices)
                               next-vertices)
                              (reduce (fn [acc [v s _]] (assoc acc v s)) vertices-to-score next-vertices)
                              (conj visited vertice))))))))

(defn do-task [lines]
  (let [[start end racetrack] (parse-lines lines)
        racegraph             (->> racetrack
                                   (map-indexed
                                    (fn [x line]
                                      (map-indexed
                                       (fn [y value]
                                         (when (= value :empty)
                                               [(when (= :empty (get-in racetrack [x (inc y)]))
                                                      [[x y] [x (inc y)] 1])
                                                (when (= :empty (get-in racetrack [(inc x) y]))
                                                      [[x y] [(inc x) y] 1])
                                                (when (= :empty (get-in racetrack [x (dec y)]))
                                                      [[x y] [x (dec y)] 1])
                                                (when (= :empty (get-in racetrack [(dec x) y]))
                                                      [[x y] [(dec x) y] 1])]))
                                       line)))
                                   (mapcat #(filter some? %))
                                   (mapcat #(filter some? %))
                                   (group-by first)
                                   (map (fn [[k v]] [k (into {} (map #(vec (drop 1 %)) v))]))
                                   (into {}))
        reference-time        (time (dijkstra start end racegraph))]
    (->> racetrack
         (map-indexed
          (fn [x line]
            (map-indexed
             (fn [y value]
               (when (= value :wall)
                     (list [[x y] [x (inc y)]]
                           [[x y] [(inc x) y]]
                           [[x y] [x (dec y)]]
                           [[x y] [(dec x) y]])))
             line)))
         (mapcat #(filter some? %))
         (mapcat
          #(filter (fn [[_ cheat-node-2]] (= :empty (get-in racetrack cheat-node-2))) %))
         (map
          (fn [[[x1 y1 :as v1] [x2 y2 :as v2]]]
            (->>
             (concat
              (list [v1 v2 1])
              (filter
               (fn [[v-adjacent _]]
                 (and (= :empty (get-in racetrack v-adjacent)) (not= v2 v-adjacent)))
               (list [[x1 (inc y1)] v1 1]
                     [[(inc x1) y1] v1 1]
                     [[x1 (dec y1)] v1 1]
                     [[(dec x1) y1] v1 1]))
              (filter
               (fn [[_ v-adjacent]]
                 (and (= :empty (get-in racetrack v-adjacent)) (not= v1 v-adjacent)))
               (list [v2 [x2 (inc y2)] 1]
                     [v2 [(inc x2) y2] 1]
                     [v2 [x2 (dec y2)] 1]
                     [v2 [(dec x2) y2] 1])))
             (group-by first)
             (map (fn [[k v]] [k (into {} (map #(vec (drop 1 %)) v))])))))
         (map #(into racegraph %))
         (map #(dijkstra start end %))
         #_(filter #(= (second %) 20))
         #_(map #(map first (first %)))
         (filter some?)
         (map #(- reference-time % 99))
         (filter pos?)
         (count))))

(time
 (with-open [rdr (clojure.java.io/reader "resources/day20.in")]
   (do-task (line-seq rdr))))

(defn distance [[x1 y1] [x2 y2]]
  (+ (if (< x1 x2) (- x2 x1) (- x1 x2))
     (if (< y1 y2) (- y2 y1) (- y1 y2))))

(defn do-task-2 [lines max-cheat-length]
  (let [[start end racetrack] (parse-lines lines)
        racegraph             (->> racetrack
                                   (map-indexed
                                    (fn [x line]
                                      (map-indexed
                                       (fn [y value]
                                         (when (= value :empty)
                                               [(when (= :empty (get-in racetrack [x (inc y)]))
                                                      [[x y] [x (inc y)] 1])
                                                (when (= :empty (get-in racetrack [(inc x) y]))
                                                      [[x y] [(inc x) y] 1])
                                                (when (= :empty (get-in racetrack [x (dec y)]))
                                                      [[x y] [x (dec y)] 1])
                                                (when (= :empty (get-in racetrack [(dec x) y]))
                                                      [[x y] [(dec x) y] 1])]))
                                       line)))
                                   (mapcat #(filter some? %))
                                   (mapcat #(filter some? %))
                                   (group-by first)
                                   (map (fn [[k v]] [k (into {} (map #(vec (drop 1 %)) v))]))
                                   (into {}))
        reference-time        (time (dijkstra start end racegraph))]
    (->> racetrack
         (map-indexed
          (fn [x line]
            (map-indexed
             (fn [y value]
               (when (= value :empty)
                     (->> (range (- (dec max-cheat-length)) (dec max-cheat-length))
                          (map
                           (fn [dx]
                             (map (fn [dy] [dx dy])
                                  (range (- (- max-cheat-length 1 dx)) (- max-cheat-length 1 dx)))))
                          (mapcat identity)
                          (remove #{[0 0]})
                          (map (fn [[dx dy]] [[x y] [(+ x dx) (+ y dy)]])))))
             line)))
         (mapcat #(filter some? %))
         (mapcat
          #(filter (fn [[_ cheat-node-2]] (= :empty (get-in racetrack cheat-node-2))) %))
         (map
          (fn [[[x1 y1 :as v1] v2]]
            (let [time-to-end             (dijkstra v2 end racegraph)
                  [v-adj time-from-start] (->> (list [x1 (inc y1)] [(inc x1) y1] [x1 (dec y1)] [(dec x1) y1])
                                               (filter #(= (get-in racetrack %) :empty))
                                               (map (fn [v-adj] [v-adj (dijkstra start v-adj racegraph)]))
                                               (filter second)
                                               (sort-by second)
                                               (first))]
              (when (and (some? time-to-end) (some? time-from-start))
                    [(+ time-from-start (distance v1 v2) (dijkstra v2 end racegraph)) v1 v2]))))
         ;         #_(filter #(= (second %) 20))
         ;         #_(map #(map first (first %)))
         (map first)
         (filter some?)
         (map #(- reference-time %))
         #_(map #(- reference-time % 49))
         (filter pos?)
         (frequencies)
         (sort)
         (reverse)
         #_(filter #(= (first %) 9)))
    #_(count (filter #{:wall} (mapcat identity racetrack)))))

(time
 (with-open [rdr (clojure.java.io/reader "resources/day20_test.in")]
   (do-task-2 (line-seq rdr) 20)))

;###############
;#...#...#.....#
;#.#.#.#.#.###.#
;#S#...#.#.#...#
;#######.#.#.###
;#######.#.#...#
;#######.#.###.#
;###..21...#...#
;###.#######.###
;#...###...#...#
;#.#####.#.###.#
;#.#...#.#.#...#
;#.#.#.#.#.#.###
;#...#...#...###
;###############