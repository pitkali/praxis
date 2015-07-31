(ns clojure-misc.shuffle)

(defn my-shuffle
  "Basic random shuffle implementation with accumulator."
  ([collection]
   (my-shuffle collection []))
  ([collection acc]
   (if (empty? collection)
     acc
     (let [[beg end] (split-at (rand-int (count collection)) collection)
           pivot (first end)]
       (recur (conj acc pivot) (concat beg (rest end)))))))

(defn my-lazy-shuffle
  "Lazy random shuffle."
  [collection]
  (if (empty? collection)
    collection
    (lazy-seq
      (let [[beg end] (split-at (rand-int (count collection)) collection)
            pivot (first end)]
        (cons pivot (my-lazy-shuffle (concat beg (lazy-seq (next end)))))))))
