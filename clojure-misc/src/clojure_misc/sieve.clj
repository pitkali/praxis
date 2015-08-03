(ns clojure-misc.sieve
  (:import (clojure.lang PersistentQueue)))

;;; Incremental Sieve of Eratosthenes as seen on
;;; http://programmingpraxis.com/2015/07/31/incremental-sieve-of-eratosthenes/

(defn- generate-primes
  "Lazy Erathosthenes' sieve implementation, used to generate primes above 3."
  [seed candidate q last-primes sieve]
  (let [next-key (fn [stride]
                   (loop [m (+ candidate stride)]
                     (if (contains? sieve m)
                       (recur (+ m stride))
                       m)))
        next-candidate (+ candidate 2)]
    (cond
      (contains? sieve candidate)
      (let [stride (sieve candidate)
            m (next-key stride)]
        (recur seed next-candidate q last-primes
               (-> sieve
                   (dissoc candidate)
                   (assoc m stride))))

      (not (== candidate q))
      (cons candidate
            (lazy-seq (generate-primes seed next-candidate q
                                       (conj last-primes candidate) sieve)))

      :else
      (let [stride (* 2 seed)
            m (next-key stride)
            new-seed (peek last-primes)]
        (recur new-seed next-candidate (* new-seed new-seed)
               (pop last-primes) (assoc sieve m stride))))))

;;; This is a function, rather than a var to reclaim memory after the caller is
;;; done with those prime numbers.
(defn primes
  "Lazy sequence of all the prime numbers generated with Eratosthenes' sieve."
  []
  (cons 2 (cons 3 (lazy-seq (generate-primes 3 5 9 PersistentQueue/EMPTY {})))))
