(ns aoc2024.day15
  (:require [clojure.string :as str])
  (:require [clojure.core.match :refer [match]])
  (:require [clojure.math :refer [log10 pow floor]])
  (:require [clojure.set :refer [union]])
  (:require [clojure.math.combinatorics :refer [cartesian-product]])
  (:require [clojure.tools.trace :as t]))

(def map-cells
  {\# :wall
   \. :empty
   \@ :empty
   \O :box})

(def direction-to-move
  {\^ [-1 0]
   \> [0 1]
   \v [1 0]
   \< [0 -1]})

(defn parse-lines [lines]
  (let [[map-lines [_ & moves-lines]] (split-with #(not (empty? %)) lines)
        wh-map                        (mapv (fn [line] (mapv map-cells line)) map-lines)
        start-pos                     (->> lines
                                           (map-indexed (fn [x str] (when-let [y (str/index-of str \@)] [x y])))
                                           (some identity))
        moves                         (map direction-to-move (mapcat identity moves-lines))]
    [start-pos wh-map moves]))

(defn eval-box-positions-sum [wh-map]
  (->> (cartesian-product (range (count wh-map)) (range (count (first wh-map))))
       (filter #(= :box (get-in wh-map %)))
       (map (fn [[x y]] (+ (* 100 x) y)))
       (apply +)))

(defn perform-move [robot-pos wh-map move]
  (loop [cell robot-pos]
    (let [next-cell (mapv + cell move)]
      (case (get-in wh-map next-cell)
            :wall  [robot-pos wh-map]
            :box   (recur (mapv + cell move))
            :empty (if (= cell robot-pos)
                     [next-cell wh-map]
                     (let [next-robot-pos (mapv + robot-pos move)]
                       [next-robot-pos
                        (-> (assoc-in wh-map next-robot-pos :empty)
                            (assoc-in next-cell :box))]))))))

(defn find-box-positions-sum [lines]
  (let [[start-pos start-wh-map moves] (parse-lines lines)]
    (loop [pos                 start-pos
           [move & moves-tail] moves
           wh-map              start-wh-map]
      (if (nil? move)
        (eval-box-positions-sum wh-map)
        (let [[new-pos new-wh-map] (perform-move pos wh-map move)]
          (recur new-pos moves-tail new-wh-map))))))

(time
 (with-open [rdr (clojure.java.io/reader "resources/day15.in")]
   (find-box-positions-sum (line-seq rdr))))

(def wide-map-cells
  {\# :wall
   \. :empty
   \@ :empty
   \[ :box-left
   \] :box-right})

(def wide-map-mapping
  {\# "##"
   \. ".."
   \@ "@."
   \O "[]"})

(def wide-map-cells-reverse
  {:wall      \#
   :empty     \.
   :box-left  \[
   :box-right \]})

(defn print-map [wh-map pos]
  (->> wh-map
       (map (fn [line] (map wide-map-cells-reverse line)))
       (map-indexed
        (fn [x line] (map-indexed (fn [y value] (if (= pos [x y]) \@ (or value \?))) line)))
       (map #(apply str %))
       (str/join "\n")
       (println)))

(defn parse-with-wide-map [lines]
  (let [[map-lines [_ & moves-lines]] (split-with #(not (empty? %)) lines)
        wide-map-lines                (map #(apply str (mapcat wide-map-mapping %)) map-lines)
        wh-map                        (mapv (fn [line] (mapv wide-map-cells line)) wide-map-lines)
        start-pos                     (->> wide-map-lines
                                           (map-indexed (fn [x str] (when-let [y (str/index-of str \@)] [x y])))
                                           (some identity))
        moves                         (map direction-to-move (mapcat identity moves-lines))]
    [start-pos wh-map (vec moves)]))

(defn perform-horizontal-move [[start-x start-y :as robot-pos] start-wh-map move-fun]
  (loop [cell       robot-pos
         wh-map     start-wh-map]
    (let [next-cell (move-fun cell)]
      (case (get-in wh-map next-cell)
            :wall      [robot-pos start-wh-map]
            :box-left  (recur next-cell (assoc-in wh-map next-cell (if (= cell robot-pos) :empty :box-right)))
            :box-right (recur next-cell (assoc-in wh-map next-cell (if (= cell robot-pos) :empty :box-left)))
            :empty     (if (= cell robot-pos)
                         [next-cell wh-map]
                         [(move-fun robot-pos)
                          (assoc-in wh-map next-cell (if (= (get-in wh-map cell) :box-left) :box-right :box-left))])))))

(defn perform-vertical-move-step [[[x y :as coord] _] wh-map move-fun]
  (let [[new-x new-y :as new-coord] (move-fun coord)]
    (case (get-in wh-map new-coord)
          :wall      [[] :break]
          :box-left  [[[new-coord (get-in wh-map coord)] [[new-x (inc new-y)] :empty]] :continue]
          :box-right [[[new-coord (get-in wh-map coord)] [[new-x (dec new-y)] :empty]] :continue]
          :empty     [[[new-coord (get-in wh-map coord)]] :finish])))

(defn apply-cells-to-wh-map [wh-map cells]
  (reduce
   (fn [wh-map-acc [coord value]] (assoc-in wh-map-acc coord value))
   wh-map
   cells))

(defn collect-to-dict [vertical-moves]
  (reduce
   (fn [acc [k v]]
     (update acc k (fn [v-old] (if (or (nil? v-old) (= :empty v-old)) v v-old))))
   {} vertical-moves))

(defn perform-cells-step [cells wh-map move-fun]
  (loop [[cell & cells-tail] cells
         cells-to-apply      (list)
         cells-to-continue   (list)]
    (if (nil? cell)
      [(collect-to-dict cells-to-apply) (collect-to-dict cells-to-continue)]
      (let [[new-cells action] (perform-vertical-move-step cell wh-map move-fun)]
        (case action
              :break    [nil nil]
              :finish   (recur cells-tail (apply conj cells-to-apply new-cells) cells-to-continue)
              :continue (recur cells-tail (apply conj cells-to-apply new-cells) (apply conj cells-to-continue new-cells)))))))

(defn perform-vertical-move [robot-pos start-wh-map move-fun]
  (loop [prev-cells-to-apply    {robot-pos :empty}
         prev-cells-to-continue {robot-pos :empty}
         wh-map                 start-wh-map]
    (let [[cells-to-apply cells-to-continue] (perform-cells-step prev-cells-to-continue wh-map move-fun)
          ;new-cells  (->> cells
          ;               (map #(perform-vertical-move-step % wh-map move-fun))
          ;               (mapcat identity)
          ;               #_(t/trace "before collect")
          ;               (collect-to-dict)
          ;               #_(t/trace "after collect"))
          new-wh-map                         (apply-cells-to-wh-map wh-map prev-cells-to-apply)]
      #_(println "to apply:" cells-to-apply)
      #_(println "to continue:" cells-to-continue)
      (cond
        (= cells-to-continue nil)  [robot-pos start-wh-map]
        (empty? cells-to-continue) [(move-fun robot-pos) (apply-cells-to-wh-map new-wh-map cells-to-apply)]
        :else                      (recur cells-to-apply cells-to-continue new-wh-map)))))

(defn perform-wide-map-move [robot-pos wh-map [move-x move-y :as move]]
  (if (= 0 move-x)
    (perform-horizontal-move robot-pos wh-map #(mapv + % move))
    (perform-vertical-move robot-pos wh-map #(mapv + % move))))

(defn eval-wide-box-positions-sum [wh-map]
  (->> (cartesian-product (range (count wh-map)) (range (count (first wh-map))))
       (filter #(= :box-left (get-in wh-map %)))
       (map (fn [[x y]] (+ (* 100 x) y)))
       (apply +)))

(defn find-wide-map-box-positions-sum [lines]
  (let [[start-pos start-wh-map moves] (parse-with-wide-map lines)]
    (print-map start-wh-map start-pos)
    (loop [pos                 start-pos
           [move & moves-tail] moves
           wh-map              start-wh-map]
      (if (nil? move)
        (eval-wide-box-positions-sum wh-map)
        (let [[new-pos new-wh-map] (perform-wide-map-move pos wh-map move)]
          #_(println move)
          #_(print-map new-wh-map new-pos)
          #_(println)
          (recur new-pos moves-tail new-wh-map))))))

(time
 (with-open [rdr (clojure.java.io/reader "resources/day15.in")]
   (find-wide-map-box-positions-sum (line-seq rdr))))
