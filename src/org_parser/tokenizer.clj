(ns org-parser.tokenizer
  (:require [clojure.string :as str]))

(def heading-regex #"^(\*+)\s+(\w*)")
(def org-directive-regex #"\s*#\+([a-zA-Z_]+)(.*)")

(defn- tokenize-plain-text
  [input-line]
  [:text input-line])

(defn- tokenize-heading
  [line]
  (let [[_ stars heading-text] (re-matches heading-regex line)
        heading-level (count stars)
        tokenized-heading (tokenize-plain-text heading-text)]
    [(keyword (str "head" heading-level)) tokenized-heading]))

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

(defn- try-tokenize-code-block
  [raw-lines]
  (let [current-line (first raw-lines)
        rest-raw-lines (rest raw-lines)
        [returned-raw-lines block-lines] (find-block rest-raw-lines #"\s*#\+END_SRC")
        [_ directive options] (re-matches org-directive-regex current-line)]
    (if (= (count returned-raw-lines) (count rest-raw-lines))
      [returned-raw-lines (tokenize-plain-text current-line)]
      [returned-raw-lines [:code-block block-lines options]])))

(defn- try-tokenize-org-directive
  [raw-lines]
  (let [current-line (first raw-lines)
        [_ directive] (re-matches org-directive-regex current-line)]
    (case directive
      "BEGIN_SRC" (try-tokenize-code-block raw-lines)
      [(rest (raw-lines)) (tokenize-plain-text current-line)])))

(defn tokenize
  [input-string]
  (loop [raw-lines (clojure.string/split-lines input-string)
         tokenized-lines []]
    (if-not (empty? raw-lines)
      (let [current-line (first raw-lines)
            [returned-raw-lines return-tokenized-line]
            (condp re-matches current-line
              heading-regex [(rest raw-lines)
                             (tokenize-heading current-line)]
              org-directive-regex (try-tokenize-org-directive raw-lines)
              [(rest raw-lines) (tokenize-plain-text current-line)])]
        (recur
         returned-raw-lines
         (conj tokenized-lines return-tokenized-line)))
      tokenized-lines)))
