(ns aoc2024.day7
  (:require [clojure.string :as str])
  (:require [clojure.core.match :refer [match]])
  (:require [clojure.set :refer [union]])
  (:require [clojure.tools.trace :as t])
  (:require [clojure.math.combinatorics :refer [cartesian-product]]))

(def operators [+ * (fn [x y] (parse-long (str x y)))])

(defn parse-input [lines]
  (->> (map #(str/split % #"\: ") lines)
       (mapv
        (fn [[result str-operands]]
          [(parse-long result)
           (mapv parse-long (str/split str-operands #" "))]))))

(defn any-operators-gave-sum? [[sum operands]]
  (->> (repeat (dec (count operands)) operators)
       (apply cartesian-product)
       (some
        (fn [operators-seq]
          (loop [[op & ops-tail]           operators-seq
                 [operand & operands-tail] (rest operands)
                 cur-sum                   (first operands)]
            (if (nil? op)
              (= sum cur-sum)
              (recur ops-tail operands-tail (op cur-sum operand))))))))

(defn find-sum-of-solvable [lines]
  (->> (parse-input lines)
       (filter any-operators-gave-sum?)
       (map first)
       (apply +)))

(time
 (with-open [rdr (clojure.java.io/reader "resources/day7.in")]
   (find-sum-of-solvable (line-seq rdr))))