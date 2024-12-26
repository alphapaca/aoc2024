(ns aoc2024.day11
  (:require [clojure.string :as str])
  (:require [clojure.core.match :refer [match]])
  (:require [clojure.math :refer [log10 pow]])
  (:require [clojure.set :refer [union]])
  (:require [clojure.tools.trace :as t]))

(defn find-neighbours [[x y :as coords] garden-map visited]
  (let [elem (get-in garden-map coords)]
    (->> (list [(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)])
         (filter #(= elem (get-in garden-map %)))
         (remove visited))))

(defn find-not-visited [garden-map visited]
  (->> garden-map
       (map-indexed (fn [x line] (map-indexed (fn [y _] [x y]) line)))
       (mapcat identity)
       (remove visited)
       (first)))

(defn find-all-regions [garden-map]
  (loop [visited #{}
         regions []
         region  #{}
         stack   (list [0 0])]
    (if (empty? stack)
      (if-let [next (find-not-visited garden-map visited)]
        (recur visited
          (conj regions region)
          #{}
          (conj stack next))
        (conj regions region))
      (let [coords (peek stack)]
        (recur (conj visited coords)
          regions
          (conj region coords)
          (apply conj (pop stack) (find-neighbours coords garden-map visited)))))))

(defn find-neighbours-in-region-count [[x y] region]
  (->> (list [(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)])
       (filter region)
       (count)))

(defn calculate-price-of-region [region]
  (*
   (count region)
   (apply -
          (* 4 (count region))
          (map #(find-neighbours-in-region-count % region) region))))

(defn calculate-price-of-fence [garden-map]
  (->> (mapv vec garden-map)
       #_(mapv #(vec (take 100 %)) (take 100 garden-map))
       (find-all-regions)
       (map calculate-price-of-region)
       (apply +)))

(time
 (with-open [rdr (clojure.java.io/reader "resources/day12.in")]
   (calculate-price-of-fence (line-seq rdr))))

(defn prev-element-for-direction [[x y] direction]
  (case direction
    :left   [(dec x) y]
    :right  [(dec x) y]
    :top    [x (dec y)]
    :bottom [x (dec y)]))

(defn find-neighbours-not-in-region-directions [[x y] region]
  (->>
   (list [[(inc x) y] :bottom]
         [[(dec x) y] :top]
         [[x (inc y)] :right]
         [[x (dec y)] :left])
   (remove (comp region first))
   (map second)))

(defn calculate-region-sides-count [region]
  (loop [[coord & tail] (sort region)
         seen-on-border #{}]
    (if (nil? coord)
      (count seen-on-border)
      (recur tail
        (let [border-directions (find-neighbours-not-in-region-directions coord region)]
          (as-> border-directions x
                (map (fn [direction] [(prev-element-for-direction coord direction) direction]) x)
                (apply disj seen-on-border x)
                (into x (map #(vector coord %) border-directions))))))))

(defn calculate-price-of-region-2 [region]
  (*
   (count region)
   (calculate-region-sides-count region)))

(defn calculate-price-of-fence-2 [garden-map]
  (->> (mapv vec garden-map)
       #_(mapv #(vec (take 100 %)) (take 100 garden-map))
       (find-all-regions)
       (map calculate-price-of-region-2)
       (apply +)))

(time
 (with-open [rdr (clojure.java.io/reader "resources/day12.in")]
   (calculate-price-of-fence-2 (line-seq rdr))))