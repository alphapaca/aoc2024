(ns aoc2024.day16
  (:require [clojure.string :as str])
  (:require [clojure.core.match :refer [match]])
  (:require [clojure.math :refer [log10 pow floor]])
  (:require [clojure.set :refer [union]])
  (:require [clojure.math.combinatorics :refer [cartesian-product]])
  (:require [clojure.tools.trace :as t])
  (:require [loom.alg :as alg])
  (:require [loom.graph :as graph]))

(def map-cells
  {\# :wall
   \. :empty
   \S :empty
   \E :empty})

(defn parse-lines [lines]
  [(->> lines
        (map-indexed (fn [x str] (when-let [y (str/index-of str \S)] [x y])))
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

(defn find-path-for-vertice [prev end-vertice]
  (loop [vertice end-vertice
         result  #{}]
    (cond
      (nil? vertice)   result
      (result vertice) (throw (IllegalStateException. "cycle found!"))
      :else            (recur (prev vertice) (conj result vertice)))))

(defn find-path-with-lowest-score [lines]
  (let [[start-pos labirynth] (parse-lines lines)]
    (loop [scores-to-vertices (sorted-map 0 [[start-pos [0 1]]])
           vertices-to-score  {[start-pos [0 1]] 0}
           prev               {}
           visited            #{}]
      (let [[[score [[pos direction :as vertice]]] & _] scores-to-vertices
            next-vertices                               (->>
                                                         (list [[(mapv + pos direction) direction] (+ 1 score)]
                                                               [[pos (if (zero? (first direction)) [1 0] [0 1])] (+ 1000 score)]
                                                               [[pos (if (zero? (first direction)) [-1 0] [0 -1])] (+ 1000 score)])
                                                         (remove (fn [[[pos _ :as v]]] (or (visited v) (= :wall (get-in labirynth pos))))))]
        (if (= pos [1 (- (count (first labirynth)) 2)])
          (do
            (let [path (find-path-for-vertice prev vertice)]
              (->>
               (map-indexed
                (fn [x line]
                  (map-indexed
                   (fn [y char]
                     (cond
                       (path [[x y] [1 0]])  \v
                       (path [[x y] [-1 0]]) \^
                       (path [[x y] [0 1]])  \>
                       (path [[x y] [0 -1]]) \<
                       :else                 char))
                   line))
                lines)
               (map #(apply str %))
               (str/join "\n")
               (println))
              score))
          (recur
            (dissoc-first
             (reduce
              (fn [acc [v s]]
                (let [s-old (vertices-to-score v)]
                  (cond
                    (nil? s-old) (assoc-multimap acc s v)
                    (<= s-old s) acc
                    :else        (assoc-multimap (dissoc-value acc s v) s v))))
              scores-to-vertices next-vertices))
            (->
             (reduce
              (fn [acc [v s]] (update acc v (fn [old-s] (min (or old-s (Long/MAX_VALUE)) s))))
              vertices-to-score next-vertices)
             (dissoc vertice))
            (reduce
             (fn [acc [v s]]
               (let [s-old (vertices-to-score v)]
                 (if (or (nil? s-old) (>= s-old s))
                   (assoc acc v vertice)
                   acc)))
             prev next-vertices)
            (conj visited vertice)))))))

(time
 (with-open [rdr (clojure.java.io/reader "resources/day16.in")]
   (find-path-with-lowest-score (line-seq rdr))))

(defn find-path-for-vertice-2 [prev end-vertice]
  (loop [[vertice & tail] (list end-vertice)
         ;vertice          end-vertice
         result           #{}]
    #_(println vertice (prev vertice))
    (if (nil? vertice)
      result
      (recur
        (apply conj tail (remove result (prev vertice)))
        #_(first (prev vertice))
        (conj result vertice)))))

(defn dijkstra [start-v end-v lines edges]
  (loop [scores-to-vertices (sorted-map 0 [start-v])
         vertices-to-score  {start-v 0}
         prev               {start-v []}
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
        (= vertice end-v) (let [path (find-path-for-vertice-2 prev vertice)]
                            #_(println path)
                            #_(->> lines
                                 (map-indexed
                                  (fn [x line]
                                    (map-indexed
                                     (fn [y char]
                                       (cond
                                         (path [[x y] :down])  \v
                                         (path [[x y] :up])    \^
                                         (path [[x y] :right]) \>
                                         (path [[x y] :left])  \<
                                         :else                 char))
                                     line)))
                                 (map #(apply str %))
                                 (str/join "\n")
                                 (println))
                            [path score])
        :else             (recur
                            (reduce
                             (fn [acc [v s old-s]]
                               (-> (if (some? old-s) (dissoc-value acc old-s v) acc)
                                   (assoc-multimap s v)))
                             (dissoc-first scores-to-vertices)
                             next-vertices)
                            (reduce (fn [acc [v s _]] (assoc acc v s)) vertices-to-score next-vertices)
                            (reduce
                             (fn [acc [v s s-old]]
                               (-> (if (= s s-old) acc (do (when (some? s-old) (println v s s-old)) (dissoc acc v)))
                                   (assoc-multimap v vertice)))
                             prev
                             next-vertices)
                            (conj visited vertice))))))

(defn do-task [lines]
  (let [[start-pos labirynth] (parse-lines lines)]
    (->> labirynth
         (map-indexed
          (fn [x line]
            (map-indexed
             (fn [y value]
               (when (= value :empty)
                 [(when (= :empty (get-in labirynth [x (inc y)]))
                    [[[x y] :right] [[x (inc y)] :right] 1])
                  (when (= :empty (get-in labirynth [(inc x) y]))
                    [[[x y] :down] [[(inc x) y] :down] 1])
                  (when (= :empty (get-in labirynth [x (dec y)]))
                    [[[x y] :left] [[x (dec y)] :left] 1])
                  (when (= :empty (get-in labirynth [(dec x) y]))
                    [[[x y] :up] [[(dec x) y] :up] 1])
                  [[[x y] :up] [[x y] :left] 1000]
                  [[[x y] :up] [[x y] :right] 1000]
                  [[[x y] :down] [[x y] :left] 1000]
                  [[[x y] :down] [[x y] :right] 1000]
                  [[[x y] :left] [[x y] :up] 1000]
                  [[[x y] :left] [[x y] :down] 1000]
                  [[[x y] :right] [[x y] :up] 1000]
                  [[[x y] :right] [[x y] :down] 1000]]))
             line)))
         (mapcat #(filter some? %))
         (mapcat #(filter some? %))
         (group-by first)
         (map (fn [[k v]] [k (into {} (map #(vec (drop 1 %)) v))]))
         (into {})
         (dijkstra [[(- (count labirynth) 2) 1] :right] [[1 (- (count (first labirynth)) 2)] :up] lines)
         (first)
         (group-by first)
         (map (fn [[k vs]] [k (mapv second vs)]))
         (into {})
         (count)
         #_(graph/weighted-graph x)
         #_(alg/dijkstra-span x [[(- (count labirynth) 2) 1] :right] [[1 (- (count (first labirynth)) 2)] :up]))))

(time
 (with-open [rdr (clojure.java.io/reader "resources/day16.in")]
   (do-task (line-seq rdr))))