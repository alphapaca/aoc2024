(ns aoc2024.day3
  (:require [clojure.string :as str]))

(defn eval-muls [lines]
  (->> lines
       (mapcat (partial re-seq #"mul\(([0-9]+)\,([0-9]+)\)"))
       (map (fn [[_ first second]] (* (parse-long first) (parse-long second))))
       (apply +)))

(with-open [rdr (clojure.java.io/reader "resources/day3.in")]
  (eval-muls (line-seq rdr)))

(defn parse-mul-or-cond [[all-match _ first-operand second-operand]]
  (cond (str/starts-with? all-match "mul") [:mul (parse-long first-operand) (parse-long second-operand)]
        (= all-match "don't()")            [:don't]
        (= all-match "do()")               [:do]
        :else                              (throw (IllegalArgumentException. (str "unknown match type: " all-match)))))

(defn reduce-muls-with-cond [[acc enabled] [op first second]]
  (case op
    :do    [acc true]
    :don't [acc false]
    :mul   [(if enabled (+ acc (* first second)) acc) enabled]))

(defn eval-muls-with-cond [lines]
  (->> lines
       (mapcat (partial re-seq #"(mul\(([0-9]+),([0-9]+)\)|do\(\)|don't\(\))"))
       (map parse-mul-or-cond)
       (reduce reduce-muls-with-cond [0 true])))

(with-open [rdr (clojure.java.io/reader "resources/day3.in")]
  (eval-muls-with-cond (line-seq rdr)))