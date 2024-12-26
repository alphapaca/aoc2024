(ns aoc2024.day9
  (:require [clojure.string :as str])
  (:require [clojure.core.match :refer [match]])
  (:require [clojure.set :refer [union]])
  (:require [clojure.tools.trace :as t]))

(defn parse-disk [line]
  (->> (map #(- (int %) (int \0)) line)
       (map-indexed (fn [idx val] [idx val]))
       (mapcat
        (fn [[idx val]]
          (if (even? idx) (repeat val (quot idx 2)) (repeat val :empty))))
       (vec)))

(defn compact-disk [start-disk]
  (loop [idx     0
         end-idx (dec (count start-disk))
         disk    start-disk]
    (cond
      (= idx end-idx)           (take (inc idx) disk)
      (= (disk end-idx) :empty) (recur idx (dec end-idx) disk)
      (not= (disk idx) :empty)  (recur (inc idx) end-idx disk)
      :else                     (recur (inc idx) (dec end-idx) (assoc disk idx (disk end-idx))))))

(defn evaluate-checksum [disk]
  (->> (map-indexed (fn [idx num] (* idx num)) disk)
       (apply +')))

(defn do-task-1 [lines]
  (->> (parse-disk (first lines))
       (compact-disk)
       (evaluate-checksum)))

(time
 (with-open [rdr (clojure.java.io/reader "resources/day9.in")]
   (do-task-1 (line-seq rdr))))

(defn parse-disk-spans [line]
  (->> (map #(- (int %) (int \0)) line)
       (map-indexed
        (fn [idx val] (if (even? idx) [:file val (quot idx 2)] [:empty val])))
       (vec)))

(defn find-file-idx-with-next-id [disk-spans last-idx last-id]
  (loop [idx (dec last-idx)]
    (if (= (get (disk-spans idx) 2) (dec last-id))
      idx
      (recur (dec idx)))))

(defn compact-disk-2 [start-disk-spans]
  (loop [file-idx (if (even? (dec (count start-disk-spans)))
                    (dec (count start-disk-spans))
                    (- (count start-disk-spans) 2))
         disk     start-disk-spans]
    (let [[file-type file-size id] (disk file-idx)
          hole                     (->> (map-indexed #(vector %1 %2) disk)
                                        (take file-idx)
                                        (filter (fn [[idx [type size]]] (and (= type :empty) (<= file-size size))))
                                        (first))]
      (cond
        (= id 0)    disk
        (nil? hole) (recur (find-file-idx-with-next-id disk file-idx id) disk)
        :else       (let [[hole-idx
                           [_ hole-size]] hole
                          new-disk        (if (= hole-size file-size)
                                            (assoc disk hole-idx (disk file-idx) file-idx (disk hole-idx))
                                            (-> (subvec disk 0 hole-idx)
                                                (conj (disk file-idx) [:empty (- hole-size file-size)])
                                                (into (subvec disk (inc hole-idx) file-idx))
                                                (conj [:empty file-size])
                                                (into (subvec disk (inc file-idx)))))]
                      (recur (find-file-idx-with-next-id new-disk file-idx id) new-disk))))))

#_(defn reduce-files-to-leftmost-holes [disk-spans [file-idx file]]
    (let [[_ file-size]            file
          [hole-idx [_ hole-size]] (->> (map-indexed (apply vector) disk-spans)
                                        (take file-idx)
                                        (some (fn [idx [type size]] (and (= type :empty) (<= file-size size)))))]
      (cond
        (nil? hole-idx)         disk-spans
        (= file-size hole-size) (assoc disk-spans hole-idx (disk-spans file-idx) file-idx (disk-spans hole-idx))
        (< file-size hole-size))))

#_(defn compact-disk-2 [start-disk-spans]
    (->> start-disk-spans
         (map-indexed (apply vector))
         (filter (fn [[idx [type]]] (= type :file)))
         (reverse)
         (reduce reduce-files-to-leftmost-holes start-disk-spans)))

#_(defn evaluate-checksum-2 [disk]
    (->> (map-indexed #(vector %1 %2) disk)
         (filter (fn [[_ id_or_empty]] (not= id_or_empty :empty)))
         (map (fn [[idx id]] (* idx id)))
         (apply +')))

(defn do-task-2 [lines]
  (->> (parse-disk-spans (first lines))
       (compact-disk-2)
       (mapcat (fn [[type size id]] (repeat size (if (= type :file) id 0))))
       (evaluate-checksum)))

(time
 (with-open [rdr (clojure.java.io/reader "resources/day9.in")]
   (do-task-2 (line-seq rdr))))

; 00992111777.44.333....5555.6666.....8888