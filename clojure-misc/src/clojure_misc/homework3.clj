(ns clojure-misc.homework3
  (:require [clojure.string :as string]
            [clojure.math.numeric-tower :as math]))

;;; Three homework problems as seen on http://programmingpraxis.com/2015/08/04/three-homework-problems/

(defn problem-1
  "Return sum of squares of 2 largest numbers of the 3."
  [x y z]
  (let [largest (max x y z)
        other (remove #(== largest %) [x y z])
        largest2 (if (< (count other) 2) largest (apply max other))]
    (+ (* largest largest) (* largest2 largest2))))

(defn is-10-palindrome
  "Is provided number a base-10 palindrome?"
  [x]
  (let [s (format "%d" x)]
    (= s (string/reverse s))))

(defn divides?
  "Does x divide n?"
  [x n]
  (== (mod n x) 0))

(defn factorial-trailing-zeroes
  [n]
  (let [numbers (range 2 (inc n))
        tens (math/floor (/ n 10))
        even (filter even? numbers)
        fives (->> numbers
                   (filter #(divides? 5 %))
                   (remove #(divides? 10 %)))]
    (+ tens (min (count even) (count fives)))))

(defn factorial [n]
  (reduce * (range 2 (inc n))))
