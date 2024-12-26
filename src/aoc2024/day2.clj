(ns aoc2024.day2
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (->> (str/split line #"\s+")
       (map parse-long)))

(defn safe-report? [safe-range-start safe-range-end report]
  (->> (partition 2 1 report)
       (map (partial apply -))
       (every? #(<= safe-range-start % safe-range-end))))

(defn evaluate-safe-reports-count [lines]
  (->> (map parse-line lines)
       (filter #(or (safe-report? 1 3 %) (safe-report? -3 -1 %)))
       (count)))

(with-open [rdr (clojure.java.io/reader "resources/day2.in")]
  (evaluate-safe-reports-count (line-seq rdr)))

(defn safe-report-with-dampener? [safe-range-start safe-range-end report]
  (->> (partition 2 1 report)
       (map-indexed (fn [idx [left right]] [idx (- left right)]))
       (every?
        (fn [[idx diff]]
          (or (<= safe-range-start diff safe-range-end)
              (safe-report? safe-range-start safe-range-end (concat (take idx report) (drop (inc idx) report)))
              (safe-report? safe-range-start safe-range-end (concat (take (inc idx) report) (drop (+ idx 2) report))))))))

(defn evaluate-safe-reports-with-dampener-count [lines]
  (->> (map parse-line lines)
       (filter #(or (safe-report-with-dampener? 1 3 %) (safe-report-with-dampener? -3 -1 %)))
       (count)))

(with-open [rdr (clojure.java.io/reader "resources/day2.in")]
  (evaluate-safe-reports-with-dampener-count (line-seq rdr)))