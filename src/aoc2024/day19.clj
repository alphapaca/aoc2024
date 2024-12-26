(ns aoc2024.day19
  (:require [clojure.string :as str])
  (:require [clojure.core.match :refer [match]])
  (:require [clojure.math :refer [log10 pow floor]])
  (:require [clojure.set :refer [union]])
  (:require [clojure.math.combinatorics :refer [cartesian-product]])
  (:require [clojure.tools.trace :as t]))

(defn parse-input [lines]
  (let [[towels-str _ & designs] lines
        towels                   (->> (str/split towels-str #", ")
                                      (into #{}))]
    [towels designs]))

#_(defn design-possible? [design towels]
    (println design)
    (loop [design-rest design
           towel-size  1
           stack       (list)]
      (println design-rest towel-size stack)
      (cond
        (empty? design-rest)                     true
        (< (count design-rest) towel-size)       (if (empty? stack)
                                                   false
                                                   (recur
                                                     (subs design (- (count design) (count design-rest) (first stack)))
                                                     (inc (first stack))
                                                     (rest stack)))
        (towels (subs design-rest 0 towel-size)) (recur (subs design-rest towel-size) 1 (conj stack towel-size))
        :else                                    (recur design-rest (inc towel-size) stack))))

(def design-possible?
  (memoize
   (fn [design towels]
     (loop [towel-size 1]
       (cond
         (empty? design)                     true
         (< (count design) towel-size)       false
         (towels (subs design 0 towel-size)) (if (design-possible? (subs design towel-size) towels)
                                               true
                                               (recur (inc towel-size)))
         :else                               (recur (inc towel-size)))))))

(defn do-task [lines]
  (let [[towels designs] (parse-input lines)]
    (->> designs
         (filter #(design-possible? % towels))
         (count))))

(time
 (with-open [rdr (clojure.java.io/reader "resources/day19.in")]
   (do-task (line-seq rdr))))

(def towels
  (with-open [rdr (clojure.java.io/reader "resources/day19.in")]
    (first (parse-input (line-seq rdr)))))

(def count-variants-for-design
  (memoize
   (fn [design towels]
     (loop [towel-size   1
            variants-sum 0]
       (cond
         (empty? design)                     1
         (< (count design) towel-size)       variants-sum
         (towels (subs design 0 towel-size)) (recur
                                               (inc towel-size)
                                               (+ variants-sum (count-variants-for-design (subs design towel-size) towels)))
         :else                               (recur (inc towel-size) variants-sum))))))

(defn do-task-2 [lines]
  (let [[towels designs] (parse-input lines)]
    (->> designs
         (map #(count-variants-for-design % towels))
         (apply +))))


(time
 (with-open [rdr (clojure.java.io/reader "resources/day19.in")]
   (do-task-2 (line-seq rdr))))