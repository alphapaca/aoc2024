(ns aoc2024.day5
  (:require [clojure.string :as str])
  (:require [clojure.core.match :refer [match]])
  (:require [clojure.set :refer [union]])
  (:require [clojure.tools.trace :as t]))

(defn parse-input [lines]
  (let [[order-rule-lines [_ & pages-to-check-lines]] (split-with (partial not= "") lines)]
    [(->> (map #(str/split % #"\|") order-rule-lines)
          (mapv (partial mapv parse-long)))
     (->> (map #(str/split % #",") pages-to-check-lines)
          (mapv (partial mapv parse-long)))]))

(defn correct-update? [ordering-rules update]
  (loop [[head-page & tail-pages] update
         should-be-prev-pages     #{}]
    (cond
     (nil? head-page)                           true
     (contains? should-be-prev-pages head-page) false
     :else                                      (recur
                                                  tail-pages
                                                  (union should-be-prev-pages
                                                         (ordering-rules head-page))))))

(defn do-task-1 [lines]
  (let [[ordering-rules-list pages-to-check] (parse-input lines)
        ordering-rules                       (as-> (group-by second ordering-rules-list) x
                                                   (zipmap (keys x)
                                                           (map #(set (mapv first %)) (vals x))))]
    (->> pages-to-check
         (filter (partial correct-update? ordering-rules))
         (map #(% (quot (count %) 2)))
         (apply +))))

(with-open [rdr (clojure.java.io/reader "resources/day5.in")]
  (do-task-1 (line-seq rdr)))

(defn find-cliques [vertices edges]
  (loop [[head & tail] vertices
         cliques       []]
    (if (nil? head)
      (t/trace cliques)
      (let [edges-for-head (edges head)
            cliques-mapped (map #(if (some % edges-for-head) (conj % head) %) cliques)]
        (recur
          tail
          (if (= cliques cliques-mapped)
            (conj cliques #{head})
            cliques-mapped))))))

(defn sort-inside-clique [update clique straight-ordering]
  (->> (map-indexed vector update)
       (filter (fn [[_ val]] (clique val)))
       (sort
        (fn [[idx1 val1] [idx2 val2]]
          (cond
           (contains? (straight-ordering val1) val2) true
           (contains? (straight-ordering val2) val1) false
           :else                                     (< idx1 idx2))))
       (map second)))

(defn fix-incorrect-update [mixed-ordering straight-ordering update]
  (reduce
   (fn [fixed-update clique]
     (sort-inside-clique fixed-update clique straight-ordering))
   update
   (find-cliques update mixed-ordering)))

(defn do-task-2 [lines]
  (let [[ordering-pairs
         pages-to-check]  (parse-input lines)
        straight-ordering (as-> (group-by first ordering-pairs) x
                                (zipmap (keys x)
                                        (map #(set (map second %)) (vals x))))
        reversed-ordering (as-> (group-by second ordering-pairs) x
                                (zipmap (keys x)
                                        (map #(set (map first %)) (vals x))))
        mixed-ordering    (->> (union (set (keys straight-ordering)) (keys reversed-ordering))
                               (map #(vector % (union (straight-ordering %) (reversed-ordering %))))
                               (into {}))]
    (->> pages-to-check
         (remove (partial correct-update? reversed-ordering))
         (map (partial fix-incorrect-update mixed-ordering straight-ordering))
         (map #(nth % (quot (count %) 2)))
         (apply +))))

(with-open [rdr (clojure.java.io/reader "resources/day5.in")]
  (do-task-2 (line-seq rdr)))