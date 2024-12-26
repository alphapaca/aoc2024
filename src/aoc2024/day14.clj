(ns aoc2024.day14
  (:require [clojure.string :as str])
  (:require [clojure.core.match :refer [match]])
  (:require [clojure.math :refer [log10 pow floor]])
  (:require [clojure.set :refer [union]])
  (:require [clojure.tools.trace :as t]))

(defn parse-positions-and-velocities [lines]
  (->> (map #(re-find #"p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)" %) lines)
       (map #(subvec % 1))
       (map #(mapv parse-long %))))

(defn calculate-position-after-n-steps [n [width height] [px py vx vy]]
  [(mod (+ px (* vx n)) width)
   (mod (+ py (* vy n)) height)])

(defn calculate-quadrant [[width height] [x y]]
  (when
    (and (not= x (quot width 2))
         (not= y (quot height 2)))
    [(quot x (inc (quot width 2)))
     (quot y (inc (quot height 2)))]))

(defn calculate-safety-factor [lines size]
  (->> (parse-positions-and-velocities lines)
       (map #(calculate-position-after-n-steps 100 size %))
       (map #(calculate-quadrant size %))
       (filter some?)
       (frequencies)
       (vals)
       (apply *)))

(time
 (with-open [rdr (clojure.java.io/reader "resources/day14.in")]
   (calculate-safety-factor (line-seq rdr) [101 103])))

(time
 (with-open [rdr (clojure.java.io/reader "resources/day14_test.in")]
   (calculate-safety-factor (line-seq rdr) [11 7])))

(defn robots-after-n-steps [lines-parsed [width height :as size] n]
  (let [coord-to-robot-count (->> (map #(calculate-position-after-n-steps n size %) lines-parsed)
                                  (frequencies))]
    (->> (repeat height (repeat width nil))
         (map-indexed
          (fn [y line] (map-indexed (fn [x _] (get coord-to-robot-count [x y] \.)) line)))
         (map (fn [line] (str/join "" line)))
         (str/join "\n"))))

(defn find-cycle-size [lines size]
  (let [start-picture (robots-after-n-steps lines size 0)]
    (loop [n 1]
      (when (zero? (rem n 1000)) (println n))
      (if (= start-picture (robots-after-n-steps lines size n))
        n
        (recur (inc n))))))

(time
 (with-open [rdr (clojure.java.io/reader "resources/day14.in")]
   (find-cycle-size (line-seq rdr) [101 103])))

(defn print-robots-after-n-steps [lines [width height :as size] n]
  (println n)
  (let [coord-to-robot-count (->> (parse-positions-and-velocities lines)
                                  (map #(calculate-position-after-n-steps n size %))
                                  (frequencies))]
    (->> (repeat height (repeat width nil))
         (map-indexed
          (fn [y line] (map-indexed (fn [x _] (get coord-to-robot-count [x y] \.)) line)))
         (map (fn [line] (str/join "" line)))
         (str/join "\n")
         (println))
    (println)
    (println)))

(defn show-robots-for-n [min-n max-n]
  (->> (range min-n max-n)
       (mapv
        #(print-robots-after-n-steps (str/split-lines (slurp "resources/day14_test.in")) [11 7] %))))

(show-robots-for-n 500 1500)

(defn spit-robots-for-n [name indices]
  (let [lines-parsed (parse-positions-and-velocities (str/split-lines (slurp "resources/day14.in")))]
    (->> indices
         (map
          (fn [n]
            (when (zero? (rem n 1000)) (println n))
            (->>
             (robots-after-n-steps
              lines-parsed
              [101 103]
              n)
             (str n \newline \newline))))
         (str/join "\n\n")
         (spit (format "day14%s.out" name))))
  nil)

(spit-robots-for-n "-vpattern" (range 81 10403 101))

(spit-robots-for-n 0 100)

; ..... 2..1.
; ..... .....
; 1.... .....
;
; ..... .....
; ...12 .....
; .1... 1....