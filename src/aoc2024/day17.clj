(ns aoc2024.day17
  (:require [clojure.string :as str])
  (:require [clojure.core.match :refer [match]])
  (:require [clojure.math :refer [log10 pow floor]])
  (:require [clojure.set :refer [union]])
  (:require [clojure.math.combinatorics :refer [cartesian-product]])
  (:require [clojure.tools.trace :as t]))

(defn combo-value [val a b c]
  (case val
        4 a
        5 b
        6 c
        val))

#_(defn do-op [op operand a b c ip]
    (case op
          0 [nil (quot a (bit-shift-left 1 (combo-value operand a b c))) b c (+ 2 ip)]
          1 [nil a (bit-xor b operand) c (+ 2 ip)]
          2 [nil a (rem (combo-value operand a b c) 8) c (+ 2 ip)]
          3 [nil a b c (if (zero? a) (+ 2 ip) operand)]
          4 [nil a (bit-xor b c) c (+ 2 ip)]
          5 [(rem (combo-value operand a b c) 8) a b c (+ 2 ip)]
          6 [nil a (quot a (bit-shift-left 1 (combo-value operand a b c))) c (+ 2 ip)]
          7 [nil a b (quot a (bit-shift-left 1 (combo-value operand a b c))) (+ 2 ip)]))

(defn compute-until-out [start-ip start-a start-b start-c program]
  (loop [ip start-ip
         a  start-a
         b  start-b
         c  start-c]
    (if (>= ip (count program))
      []
      (let [op      (program ip)
            operand (program (inc ip))]
        (if (= 5 op)
          (do
            [ip a b c (mod (combo-value operand a b c) 8)])
          (recur (if (and (= op 3) (not= 0 a)) operand (+ 2 ip))
            (if (zero? op) (quot a (bit-shift-left 1 (combo-value operand a b c))) a)
            (case op
                  1 (bit-xor b operand)
                  2 (mod (combo-value operand a b c) 8)
                  4 (bit-xor b c)
                  6 (quot a (bit-shift-left 1 (combo-value operand a b c)))
                  b)
            (if (= op 7) (quot a (bit-shift-left 1 (combo-value operand a b c))) c)))))))

(defn compute [start-a program]
  (loop [ip  0
         a   start-a
         b   0
         c   0
         out []]
    (let [[new-ip new-a new-b new-c new-out] (compute-until-out ip a b c program)]
      (if (= new-ip nil)
        out
        (recur (+ 2 new-ip) new-a new-b new-c (conj out new-out))))))

; (1 8 64 512 4096 32768 262144 2097152 16777216 134217728 1073741824 8589934592 68719476736 549755813888 4398046511104 35184372088832)
(compute 56 [2 4, 1 1, 7 5, 4 0, 0 3, 1 6, 5 5, 3 0])

(def compute-recur
  (memoize
   (fn [ip a b c program]
     (if (>= ip (count program))
       ""
       (let [op           (program ip)
             operand      (program (inc ip))
             recur-result (compute-recur
                           (if (and (= op 3) (not= 0 a)) operand (+ 2 ip))
                           (if (zero? op) (quot a (bit-shift-left 1 (combo-value operand a b c))) a)
                           (case op
                                 1 (bit-xor b operand)
                                 2 (rem (combo-value operand a b c) 8)
                                 4 (bit-xor b c)
                                 6 (quot a (bit-shift-left 1 (combo-value operand a b c)))
                                 b)
                           (if (= op 7) (quot a (bit-shift-left 1 (combo-value operand a b c))) c)
                           program)]
         (if (= op 5)
           (str (rem (combo-value operand a b c) 8) recur-result)
           recur-result))))))

; test
; 4,6,3,5,6,3,5,2,1,0
(time
 (str/join "," (compute-recur 0 729 0 0 [0 1 5 4 3 0])))

; 7,2,6,7
(time
 (str/join "," (compute 729 [2 4, 1 1, 7 5, 4 0, 0 3, 1 6, 5 5, 3 0])))

; input
; 1,6,3,6,5,6,5,1,7
(time
 (str/join "," (compute 30899381 [2, 4, 1, 1, 7, 5, 4, 0, 0, 3, 1, 6, 5, 5, 3, 0])))

(defn check-a-value-replicates [start-a program]
  (loop [ip  0
         a   start-a
         b   0
         c   0
         i   0]
    (let [[new-ip new-a new-b new-c new-out] (compute-until-out ip a b c program)]
      (cond
        (= new-ip nil)                       (= i (count program))
        (not= new-out (program i))           false
        :else                                (recur (+ 2 new-ip) new-a new-b new-c (inc i))))))

(defn find-program-replicating-value [program]
  (let [program-str (str/join "" program)]
    (loop [a-val 0]
      (when (zero? (mod a-val 1000000)) (println a-val))
      (if (= (compute-recur 0 a-val 0 0 program) program-str)
        a-val
        (recur (inc a-val))))))


; 117440
(time
 (find-program-replicating-value [0, 3, 5, 4, 3, 0]))

(time
 (find-program-replicating-value [2 4, 1 1, 7 5, 4 0, 0 3, 1 6, 5 5, 3 0]))

; out ((a mod 8) xor 1) xor (a div (1<<((a mod 8) xor 1))) xor 110
;

; bst a: b = a mod 8
; bxl 1: b = b xor 1
; cdv b: c = a div (1<<b)
; bxc:   b = b xor c
; adv 3: a = a div (1<<3)
; bxl 6: b = b xor 6
; out b:
; jnz 0:

; 0 adv combo
; 1 bxl
; 2 bst combo
; 3 jnz
; 4 bxc
; 5 out combo
; 6 bdv combo
; 7 cdv combo

#_(def compute-dedicated-program
    (memoize
     (fn [a]
       (if (zero? a)
         ""
         (let [new-a (quot a 8)
               t     (bit-xor (rem a 8) 1)]
           (str (rem (bit-xor (bit-xor t (quot a (bit-shift-left 1 t))) 6) 8)
                (compute-dedicated-program new-a)))))))

(defn compute-dedicated-program [start-a]
  (loop [a   start-a
         out ""]
    (if (zero? a)
      out
      (recur
        (quot a 8)
        (str out
             (bit-and 7
                      (bit-xor 6
                               (bit-xor
                                (bit-xor (bit-and a 7) 1)
                                (quot a (bit-shift-left 1 (bit-xor (bit-and a 7) 1)))))))))))

(defn compute-dedicated-program-first-out [a]
  (bit-and 7
           (bit-xor 6
                    (bit-xor (bit-xor (bit-and a 7) 1) (quot a (bit-shift-left 1 (bit-xor (bit-and a 7) 1)))))))

(t/deftrace find-value-for-output [[target & tail] a]
  (if (nil? target)
    a
    (loop [new-a (* 8 a)]
      (cond
        (= new-a (* 8 (inc a))) nil
        (= (compute-dedicated-program-first-out new-a) target)
        (let [recur-call-value (find-value-for-output tail new-a)]
          (if (nil? recur-call-value)
            (recur (inc new-a))
            recur-call-value))
        :else                   (recur (inc new-a))))))

(time
 (find-value-for-output (reverse [2 4, 1 1, 7 5, 4 0, 0 3, 1 6, 5 5, 3 0]) 0))

; test
; 4,6,3,5,6,3,5,2,1,0
(time
 (str/join "," (compute-dedicated-program 0 729 0 0 [0 1 5 4 3 0])))

; c = a div (1<<((a mod 8) xor 1))
; out ((a mod 8) xor 1) xor (a div (1<<((a mod 8) xor 1))) xor 6
; a = a div 8

(defn find-a-when-our-program-returns [program]
  (let [program-str (str/join "" program)]
    (loop [a 0]
      (when (zero? (mod a 10000000)) (println a))
      (if (= (compute-dedicated-program a) program-str)
        a
        (recur (inc a))))))

(time (compute-dedicated-program 56))

(time (find-a-when-our-program-returns [7 2 6 7]))

(time (find-a-when-our-program-returns [1, 6, 3, 6, 5, 6, 5, 1, 7]))

(defn )