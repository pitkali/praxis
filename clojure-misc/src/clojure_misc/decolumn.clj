(ns clojure-misc.decolumn
  (:require [clojure.string :as string]))

;;; De-columnizing problem as described here:
;;; https://www.reddit.com/r/dailyprogrammer/comments/3esrkm/20150727_challenge_225_easyintermediate/

(def examples
  ["This is an example piece of text. This is an exam-\nple piece of text. This is an example piece of\ntext. This is an example\npiece of text. This is a +-----------------------+\nsample for a challenge.  |                       |\nLorum ipsum dolor sit a- |       top class       |\nmet and other words. The |        feature        |\nproper word for a layout |                       |\nlike this would be type- +-----------------------+\nsetting, or so I would\nimagine, but for now let's carry on calling it an\nexample piece of text. Hold up - the end of the\n                 paragraph is approaching - notice\n+--------------+ the double line break for a para-\n|              | graph.\n|              |\n|   feature    | And so begins the start of the\n|   bonanza    | second paragraph but as you can\n|              | see it's only marginally better\n|              | than the other one so you've not\n+--------------+ really gained much - sorry. I am\n                 certainly not a budding author\nas you can see from this example input. Perhaps I\nneed to work on my writing skills."
   "+-------------+ One hundred and fifty quadrillion,\n|             | seventy-two trillion, six hundred\n| 150 072 626 | and twenty-six billion, eight hun-\n| 840 312 999 | dred and fourty million, three\n|             | hundred and thirteen thousand sub-\n+-------------+ tract one is a rather large prime\n                number which equals one to five if\ncalculated modulo two to six respectively.\n\nHowever, one other rather more in- +-------------+\nteresting number is two hundred    |             |\nand twenty-one quadrillion, eight  | 221 806 434 |\nhundred and six trillion, four     | 537 978 679 |\nhundred and thirty-four billion,   |             |\nfive hundred and thirty-seven mil- +-------------+\nmillion, nine hundred and seven-\n                                ty-eight thousand,\n+-----------------------------+ six hundred and\n|                             | seventy nine,\n| Subscribe for more Useless  | which isn't prime\n|      Number Facts(tm)!      | but is the 83rd\n+-----------------------------+ Lucas number."
   "+----------------+ Lorem ipsum dolor sit amet,\n|                | consectetur adipiscing elit,\n|  Aha, now you  | sed do eiusmod tempor incid-\n|  are stumped!! | idunt ut labore et dolore\n|                | magna aliqua. Ut enim ad mi-\n|       +--------+ nim veniam, quis nostrud ex-\n|  top  |          ercitation ullamco laboris\n|  kek  | nisi ut aliquip ex.\n|       |                       +-------------+\n+-------+ Duis aute irure dolor |             |\nin repre-henderit in voluptate  | Nothing to  |\nvelit esse cillum dolore eu fu- |  see here.  |\ngiat nulla pariatur. Excepteur  |             |\nsint occaecat cupidatat non     +-------------+\nproident, sunt in culpa qui of-\nficia deserunt mollit anim id est laborum."])

(defn fold-paragraph-list
  "Joins a sequence of strings with spaces. This function is aware of
  hyphenation: if line ends with -, it will be removed, and no space will be
  added between that and subsequent line."
  ;; We use string builder as an accumulator, because I insist on processing
  ;; supplied sequence only once.
  ([line-seq]
    (fold-paragraph-list line-seq (new StringBuilder)))
  ([line-seq builder]
    (if (empty? line-seq)
      (str builder)
      (let [line (first line-seq)
            remainder (next line-seq)
            length (.length line)
            ;; Below we use last, to ensure proper support for UTF-8
            last-char (last (seq line))]
        (if (= last-char \-)
          (.append builder (subs line 0 (dec length)))
          (do (.append builder line)
              (when-not (or (empty? line) (empty? remainder))
                (.append builder \space))))
        (recur remainder builder)))))

(defn process-line
  "Divides line into left feature text, main text content, and right feature text."
  [text]
  (let [feature-line "\\+-*\\+"
        feature-boundary "(\\||\\+-*\\+)"
        pattern-string (string/join
                         ["(" feature-line "|" "\\|" "(?<left>.*?)" feature-boundary ")?"
                          "(?<text>.*?)"
                          "(" feature-line "|" feature-boundary "(?<right>.*)" "\\|" ")?"])
        matcher (re-matcher (re-pattern pattern-string) text)
        matched-group #(.group matcher ^String %)
        safe-trim #(and % (string/trim %))]
    (if (.matches matcher)
      (map (comp safe-trim matched-group) ["left" "text" "right"])
      [nil "" nil])))

(defn fill-paragraph
  ([astring fill-width]
    (fill-paragraph astring fill-width []))
  ([astring fill-width lines]
   (if (<= (.length astring) fill-width)
     (string/join "\n" (conj lines astring))
     (let [last-good-space (.lastIndexOf astring (int \space) fill-width)
           line (subs astring 0 last-good-space)
           rst (subs astring (inc last-good-space))]
       (recur rst fill-width (conj lines line))))))

(def paragraph-fill-width 62)

;; First take at an easy solution
(defn decolumn-easy
  ([text]
    (decolumn-easy (string/split-lines text) [] []))
  ([lines current-paragraph paragraphs]
   (letfn [(filled-paragraph
             []
             (fill-paragraph (fold-paragraph-list current-paragraph)
                             paragraph-fill-width))
           (add-paragraph [] (conj paragraphs (filled-paragraph)))]
     (if (empty? lines)
       (string/join "\n\n" (add-paragraph))
       (let [line (first lines)
             rst (next lines)
             line-text (second (process-line line))]
         (if (empty? line-text)
           (recur rst [] (add-paragraph))
           (recur rst (conj current-paragraph line-text) paragraphs)))))))

;;; The intermediate part

(defn fold-all-paragraphs
  "Process sequence of strings into a sequence of
   [[start, end], paragraph]."
  ([line-seq boundary?]
    (fold-all-paragraphs (map vector (range) line-seq)
                         #(boundary? (% 1))
                         []))
  ([line-seq boundary? paragraphs]
    (let [text? (comp not boundary?)
          [_ next-paragraph] (split-with boundary? line-seq)
          [current-paragraph rst] (split-with text? next-paragraph)
          fold-range #(vector (first %) (inc (last %)))]
      (if (empty? current-paragraph)
        paragraphs
        (recur rst boundary?
               (conj paragraphs [(fold-range (map #(% 0) current-paragraph))
                                 (fold-paragraph-list (map #(% 1) current-paragraph))]))))))

(def text-paragraph-boundary?    empty?)
(def feature-paragraph-boundary? nil?)

;; Writing fold-all-paragraphs allows us to write the easy solution in a nicer way.
(defn decolumn-easier
  [text]
  (let [lines (string/split-lines text)
        contents (map (comp second process-line) lines)
        paragraphs (fold-all-paragraphs contents
                                        text-paragraph-boundary?)]
    (string/join "\n\n" (map #(fill-paragraph (% 1) paragraph-fill-width) paragraphs))))
