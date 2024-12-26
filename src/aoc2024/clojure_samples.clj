(ns aoc2024.clojure-samples
  (:require [clojure.string :as str])
  (:require [clojure.math]))

(comment
;; Numeric types
  18        ; integer
  -1.8      ; floating point
  0.18e2    ; exponent
  18.0M     ; big decimal
  18/324    ; ratio
  18N       ; big integer
  0x12      ; hex
  022       ; octal
  2r10010   ; base 2

;; Character types
  "hello"         ; string
  \e              ; character
  #"[0-9]+"       ; regular expression

;; Symbols and idents
  map             ; symbol
  +               ; symbol - most punctuation allowed
  clojure.core/+  ; namespaced symbol
  nil             ; null/nil value (named in the LISP tradition)
  true false      ; booleans
  :alpha          ; keyword
  )

(comment
  ;; = COLLECTIONS =
  ;; Clojure has literal syntax for four collection types
  ;; They evaluate to themselves.

  '(1 2 3)     ; list (a quoted list, more about this below)
  [1 2 3]      ; vector
  #{1 2 3}     ; set
  {:a 1 :b 2}  ; map

  ;; They compose

  {:foo [1 2]
   :bar #{1 2}}

  ;; In Clojure we do most things with just these
  ;; collections. Literal collections and functions.
  )

(comment
  ;; = FUNCTIONS AND CONST = 

  (+ 2 2) ; 2 + 2
  (println "Hello" "world") ; println("Hello", "world") 

  (def foo :foo)

  foo

  (defn add2 [arg]
    (+ arg 2))

  (add2 2)

  (add2 "2") ; no javascript in jvm 
  )

(comment
  ;; = LAMBDAS =

  (def add2 (fn [arg] (+ arg 2)))

  (def add2 #(+ % 2))

  (add2 2)

  (def div #(/ %1 %2))

  (div 10 2))

(comment
  ;; = local const =
  (let [y 2
        z 3]
    [y z])

  (if-let [y nil]
    :true
    :false))

(comment
  ;; = if cond case =
  (if (= (str 5) "5") :true :false)

  (let [dice-roll (inc (rand-int 6))]
    (cond
      (= 6 dice-roll)  "Six is as high as it gets"
      (odd? dice-roll) (str "An odd roll " dice-roll " is")
      :else            (str "Not six, nor odd, instead: " dice-roll)))

  (let [test-str "foo bar"]
    (case test-str
      "foo bar" (str "That's very " :foo-bar)
      "baz"     :baz
      (count    test-str))))

(comment
    ;; == `for` ==
  ;; The `for` macro really demonstrates how Clojure
  ;; can be extended using Clojure. You might think
  ;; it provides looping like the for loop in many
  ;; other languages, but in Clojure there are no for
  ;; loops. Instead `for` is about list comprehensions
  ;; (if you have Python experience, yes, that kind of
  ;; list comprehensions). Here's how to produce the
  ;; cartesian product of two vectors, `x` and `y`:

  (for [x [1 2 3]
        y [1 2 3 4]]
    [x y])

  ;; If you recall the `let` form above, and how it
  ;; lets you bind variables to use in the body of the
  ;; form, this is similar, only that `x` and `y` will
  ;; get bound to each value in the sequences and the
  ;; body will get evaluated for all combinations of
  ;; `x` and `y`.

  ;; All values? Well, `for` also lets you filter the
  ;; results

  (for [x [1 2 3]
        y [1 2 3 4]
        :when (not= x y)]
    [x y])

  ;; You can bind variable names in the comprehension
  ;; to store intermediate calculations and generally
  ;; make code more readable

  (for [x [1 2 3]
        y [1 2 3 4]
        :let [d' (- x y)
              d (Math/abs d')]]
    d)

  ;; Is the same as:

  (for [x [1 2 3]
        y [1 2 3 4]]
    (Math/abs (- x y))))

(comment
  ;; more collections
  
  (:d {:c 3 :d 4})

  ({:c 3 :d 4} :d)

  ([3 4] 1)

  ([3 4] 2)

  (get [3 4] 2)

  (#{3 4} 4)

  (#{3 4} 5)

  (conj #{3 4} 5)

  (conj [3 4] 5)

  (conj (list 3 4) 5)

  (assoc [3 4] 1 5)

  (assoc {:a 1 :d 4} :a 2 :b 3)

  ; persistent

  (def v [3 4])

  (assoc [3 4] 1 5)
  )

(comment
  ;; == threading operators ==

  (->> [1 1 2 3 5 8 13 21]
       (partition 2)
       (zipmap [:a :b :c :d])
       :d
       (apply -)
       (abs))

  (as-> [1 1 2 3 5 8 13 21] x
    (partition 2 x)
    (zipmap [:a :b :c :d] x)
    (x :d)
    (apply - x)
    (abs x)))

(comment
  ;; loop recur
  (defn factorial [n]
    (if (zero? n)
      1
      (* n (factorial (dec n)))))

  (factorial 5)

  (defn factorial-2 [n result]
    (if (zero? n)
      result
      (recur (dec n) (* n result))))

  (factorial-2 5 1)

  (defn factorial-3 [n]
    (loop [i 2
           result 1]
      (if (> i n)
        result
        (recur (inc i)
               (* result i)))))

  (factorial-3 5))

(comment
  ;; destructuring

  (defn foo [[head & tail]]
    [head tail])

  (foo [1 2 3 4])

  (defn sum [list]
    (loop [[head & tail] list
           result 0]
      (if (nil? head)
        result
        (recur tail
               (+ head result)))))

  (sum [1 2 3 4 5])
  
  (defn sum-pairs [list]
    (loop [[[x y :as head] & tail] list
           result []]
      (if (nil? head)
        result
        (recur tail
               (conj result (+ x y))))))
  
  (sum-pairs [[1 2] [3 4] [5 6]])

  (map (fn [[x y]] (+ x y)) [[1 2] [3 4] [5 6]])

  (map #(apply + %) [[1 2] [3 4] [5 6]])
  )

(comment
  ;; IO
  
  (slurp "resources/day1.in")
  
  (str/split-lines (slurp "resources/day1.in"))
  
  (spit "hello.out" "Hello"))