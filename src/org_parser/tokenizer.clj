(ns org-parser.tokenizer
  (:require [clojure.string :as str]))

(def title-directive"#+TITLE:")
(def level-1-heading "* ")
(def level-2-heading "** ")
(def level-3-heading "*** ")
(def level-4-heading "**** ")
(def level-5-heading "***** ")
(def level-6-heading "****** ")

(defn- tokenize-plain-text
  [input-line]
  [:text input-line])

(defn- starts-with-substr?
  "Exchanges order of argument than standard starts-with?"
  [substr s]
  (str/starts-with? s substr))

(defn- tokenize-line
  [line]
  (condp starts-with-substr? line
    level-1-heading [:head1 (tokenize-plain-text (subs line 2))]
    level-2-heading [:head2 (tokenize-plain-text (subs line 3))]
    level-3-heading [:head3 (tokenize-plain-text (subs line 4))]
    level-4-heading [:head4 (tokenize-plain-text (subs line 5))]
    level-5-heading [:head5 (tokenize-plain-text (subs line 6))]
    level-6-heading [:head6 (tokenize-plain-text (subs line 7))]
    (tokenize-plain-text line)))

(defn tokenize
  [input-string]
  (let [input-lines (clojure.string/split-lines input-string)]
    (map tokenize-line input-lines)))
