(ns aoc2024.day13
  (:require [clojure.string :as str])
  (:require [clojure.core.match :refer [match]])
  (:require [clojure.math :refer [log10 pow floor]])
  (:require [clojure.set :refer [union]])
  (:require [clojure.tools.trace :as t]))

(defn parse-claw-machine [[button-a-line button-b-line prize-line]]
  [(mapv parse-long (subvec (re-find #"Button A: X\+(\d+), Y\+(\d+)" button-a-line) 1))
   (mapv parse-long (subvec (re-find #"Button B: X\+(\d+), Y\+(\d+)" button-b-line) 1))
   (mapv #(+ 10000000000000 (parse-long %))
         (subvec (re-find #"Prize: X\=(\d+), Y\=(\d+)" prize-line) 1))])

(defn parse-input [lines]
  (->> (partition 3 4 lines)
       (map parse-claw-machine)))

#_(defn calculate-solution-cost [[x y]]
  (+ (* 3 x) y))

#_(defn new-solution-minimal [prev new]
  (if (nil? prev)
    new
    (< (calculate-solution-cost new) (calculate-solution-cost prev))))

#_(defn solve-claw-machine [[[ax ay] [bx by] [tx ty]]]
  (loop [ap            0
         best-solution nil]
    (if (< tx (* ax ap))
      best-solution
      (let [bx*bp (- tx (* ax ap))
            bp    (when (= 0 (rem bx*bp bx)) (quot bx*bp bx))]
        (if (and (some? bp)
                 (= ty (+ (* ap ay) (* bp by))
                 (new-solution-minimal best-solution [ap bp])))
          (recur (inc ap) [ap bp])
          (recur (inc ap) best-solution))))))

; y = (c2 - c1*a2/a1)/(b2 - a2*b1/a1)
; x = (c1 - b1 * y) / a1
(defn solve-claw-machine [[[a1 a2] [b1 b2] [c1 c2]]]
  (let [y (/ (- c2 (/ (* c1 a2) a1)) (- b2 (/ (* a2 b1) a1)))
        x (/ (- c1 (* b1 y)) a1)]
    (when (zero? (rem x 1)) (zero? (rem y 1))
      [(long x) (long y)])))

(defn calculate-min-cost-for-max-prizes [lines]
  (->> (parse-input lines)
       (map solve-claw-machine)
       (filter some?)
       (map calculate-solution-cost)
       (apply +)))

(time
 (with-open [rdr (clojure.java.io/reader "resources/day13.in")]
   (calculate-min-cost-for-max-prizes (line-seq rdr))))