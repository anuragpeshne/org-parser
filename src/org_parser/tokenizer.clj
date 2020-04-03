(ns org-parser.tokenizer)

(def org-title-directive"#+TITLE:")
(def org-level-1-heading "*")
(def org-level-2-heading "**")
(def org-level-3-heading "***")
(def org-level-4-heading "****")
(def org-level-5-heading "*****")
(def org-level-6-heading "******")

(defn tokenize
  [string]
  (clojure.string/split-lines string))
