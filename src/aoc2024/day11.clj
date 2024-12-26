(ns aoc2024.day11
  (:require [clojure.string :as str])
  (:require [clojure.core.match :refer [match]])
  (:require [clojure.math :refer [log10 pow]])
  (:require [clojure.set :refer [union]])
  (:require [clojure.tools.trace :as t]))

(set! *warn-on-reflection* true)

(defn get-digits [n]
  (loop [result (list)
         rest-n n]
    (if (= rest-n 0)
      result
      (recur (conj result (rem rest-n 10)) (quot rest-n 10)))))

(defn digits-to-number [digits]
  (loop [result         0
         [digit & tail] (reverse digits)
         multiplier     1]
    (if (nil? digit)
      result
      (recur (+ result (* multiplier digit)) tail (* 10 multiplier)))))

#_(def rules
    [(fn [n] (when (= n 0) (list 1)))
     (fn [n]
       (let [n-digits       (get-digits n)
             n-digits-count (count n-digits)]
         (when (even? n-digits-count)
           (list (digits-to-number (take (quot n-digits-count 2) n-digits))
                 (digits-to-number (drop (quot n-digits-count 2) n-digits))))))
     (fn [n] (list (* n 2024)))])

#_(def rules
    [(fn [n] (when (= n 0) (list 1)))
     (fn [n]
       (let [str-n  (str n)
             strlen (.length ^String str-n)]
         (when (even? strlen)
           (list (Long/valueOf (subs str-n 0 (quot strlen 2)))
                 (Long/valueOf (subs str-n (quot strlen 2)))))))
     (fn [n] (list (* n 2024)))])

#_(defn apply-rules [n]
  (if (= n 0)
    (list 1)
    (let [num-size (long (inc (log10 n)))]
      (if (even? num-size)
        (let [pow-10 (long (pow 10 (quot num-size 2)))]
          (list (quot n pow-10) (rem n pow-10)))
        (list (* n 2024))))))

(defn apply-rules [n]
    (if (= n 0)
      (list 1)
      (let [str-n  (str n)
            strlen (.length ^String str-n)]
        (if (even? strlen)
          (list (Long/valueOf (subs str-n 0 (quot strlen 2)))
                (Long/valueOf (subs str-n (quot strlen 2))))
          (list (* n 2024))))))

(defn make-iteration [stones]
  (loop [[stone & tail] stones
         result         (transient [])]
    (if (nil? stone)
      (persistent! result)
      (recur tail (if (= stone 0)
                    (conj! result 1)
                    (let [num-size (long (inc (log10 stone)))]
                      (if (even? num-size)
                        (let [pow-10 (long (pow 10 (quot num-size 2)))]
                          (conj! (conj! result (quot stone pow-10)) (rem stone pow-10)))
                        (conj! result (* stone 2024)))))))))

(defn find-stones-count [input]
    (-> (iterate #(make-iteration %) input)
        (nth 30)
        (count)))

(def find-count-for-stone
  (memoize
   (fn [stone iterations-left]
     (cond
       (= 0 iterations-left)              1
       (= 0 stone)                        (find-count-for-stone 1 (dec iterations-left))
       (even? (long (inc (log10 stone)))) (let [pow-10 (long (pow 10 (quot (inc (log10 stone)) 2)))]
                                            (+ (find-count-for-stone (quot stone pow-10) (dec iterations-left))
                                               (find-count-for-stone (rem stone pow-10) (dec iterations-left))))
       :else                              (find-count-for-stone (* 2024 stone) (dec iterations-left))))))

#_(defn find-stones-count [input]
    (->> input
         (map #(find-count-for-stone % 75))
         (apply +)))

(def day11input [92 0 286041 8034 34394 795 8 2051489])

(time (find-stones-count day11input))