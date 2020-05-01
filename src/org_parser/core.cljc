(ns org-parser.core
  (:require [org-parser.tokenizer :refer [tokenize]]
            [org-parser.parser :refer [parse]]
            [org-parser.emitter :refer [to-html]]))

(defn compile-to-html
  [org-file]
  (-> org-file
      tokenize
      parse
      to-html))
