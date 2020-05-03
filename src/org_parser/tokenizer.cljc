(ns org-parser.tokenizer
  (:require [clojure.string :as str]))

(def heading-regex #"^(\*+)\s+(.+)")
(def example-block-regex #"\s*#\+END_EXAMPLE")
(def code-block-regex #"\s*#\+END_SRC")
(def verse-block-regex #"\s*#\+END_VERSE")
(def unordered-list-regex #"(\s*)([-+*]) (.*)")
(def ordered-list-regex #"(\s*)([0-9]+[.\)]) (.*)")
(def org-directive-regex #"\s*#\+([a-zA-Z_]+)(.*)")
(def inline-format-capture-regex {:bold #"\*(.+?\S)\*(.*)"
                                  :italic #"/(.+?\S)/(.*)"
                                  :inline-code #"~(.+?\S)~(.*)"
                                  :underline #"_(.+?\S)_(.*)"
                                  :strikethrough #"\+(.+?\S)\+(.*)"
                                  :verbatim #"=(.+?\S)=(.*)"
                                  :link #"[([.*])([.*])]"})

(defn- tokenize-plain-text
  [input-line]
  [:text input-line])

(defn- try-capture-until
  [chars format-type]
  (let [until-regex (inline-format-capture-regex format-type)
        [_ formatted-text rest-chars] (re-matches until-regex (str/join chars))]
    (if (nil? formatted-text)
      [chars []]
      [rest-chars [format-type formatted-text]])))

(defn- tokenize-inline-formatting
  [raw-chars]
  (loop [unprocessed-chars raw-chars
         captured-chars []
         plain-text-chars []]
    (if (empty? unprocessed-chars)
      (if (empty? plain-text-chars)
        captured-chars
        (conj captured-chars [:text (str/join plain-text-chars)]))
      (let [current-char (first unprocessed-chars)
            rest-unprocessed-chars (rest unprocessed-chars)
            char-capture-regex-mapping {\* :bold
                                        \/ :italic
                                        \_ :underline
                                        \~ :inline-code
                                        \= :verbatim
                                        \+ :strikethrough
                                        \[ :link}
            [returned-unprocessed-chars returned-captured-chars]
            (if-let [format-type (char-capture-regex-mapping current-char)]
              (try-capture-until unprocessed-chars format-type)
              [unprocessed-chars []])]
        (if (empty? returned-captured-chars)
          (recur rest-unprocessed-chars
                 captured-chars
                 (conj plain-text-chars current-char))
          (recur returned-unprocessed-chars
                 (conj captured-chars [:text (str/join plain-text-chars)] returned-captured-chars)
                 []))))))

(defn- tokenize-paragraph
  [input-line]
  [:paragraph (tokenize-inline-formatting input-line)])

(defn- tokenize-heading
  [line]
  (let [[_ stars heading-text] (re-matches heading-regex line)
        heading-level (count stars)
        tokenized-heading (tokenize-inline-formatting heading-text)]
    [:head heading-level tokenized-heading]))

(defn- tokenize-list
  [line capturing-regex list-type]
  (let [[_ spaces list-symbol list-text] (re-matches capturing-regex line)
        indentation (count spaces)
        tokenized-list-text (tokenize-inline-formatting list-text)]
    [list-type indentation list-symbol tokenized-list-text]))

(defn- find-block
  [raw-lines end-regex]
  (loop [unprocessed-lines raw-lines
         captured-lines []]
    (if (empty? unprocessed-lines)
      [raw-lines []] ; we reached EOF without reaching end-regex
      (let [current-line (first unprocessed-lines)
            rest-unprocessed-lines (rest unprocessed-lines)]
        (if (re-matches end-regex current-line)
          [rest-unprocessed-lines captured-lines]
          (recur rest-unprocessed-lines (conj captured-lines current-line)))))))

(defn- try-tokenize-block
  [raw-lines block-keyword end-regex]
  (let [current-line (first raw-lines)
        rest-raw-lines (rest raw-lines)
        [returned-raw-lines block-lines] (find-block rest-raw-lines end-regex)
        [_ _ options] (re-matches org-directive-regex current-line)]
    (if (= (count returned-raw-lines) (count rest-raw-lines))
      [returned-raw-lines (tokenize-paragraph current-line)]
      [returned-raw-lines [block-keyword options block-lines]])))

(defn- try-tokenize-org-directive
  [raw-lines]
  (let [current-line (first raw-lines)
        [_ directive] (re-matches org-directive-regex current-line)]
    (case directive
      "BEGIN_SRC" (try-tokenize-block raw-lines :code-block code-block-regex)
      "BEGIN_EXAMPLE" (try-tokenize-block raw-lines :example-block example-block-regex)
      "BEGIN_VERSE" (try-tokenize-block raw-lines :verse-block verse-block-regex)
      [(rest (raw-lines)) (tokenize-plain-text current-line)])))

(defn tokenize
  [input-string]
  (loop [raw-lines (clojure.string/split-lines input-string)
         tokenized-lines []]
    (if-not (empty? raw-lines)
      (let [current-line (first raw-lines)
            rest-raw-lines (rest raw-lines)
            [returned-raw-lines return-tokenized-line]
            (condp re-matches current-line
              heading-regex [rest-raw-lines
                             (tokenize-heading current-line)]
              org-directive-regex (try-tokenize-org-directive raw-lines)
              unordered-list-regex [rest-raw-lines (tokenize-list current-line unordered-list-regex :ulist)]
              ordered-list-regex [rest-raw-lines (tokenize-list current-line ordered-list-regex :olist)]
              [rest-raw-lines (tokenize-paragraph current-line)])]
        (recur
         returned-raw-lines
         (conj tokenized-lines return-tokenized-line)))
      tokenized-lines)))
