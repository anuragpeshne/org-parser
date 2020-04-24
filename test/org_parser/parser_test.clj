(ns org-parser.parser-test
  (:require [org-parser.parser :as parser]
            [org-parser.tokenizer :as tok]
            [clojure.test :as t]))

(deftest heading-test
  (testing "Simple h1"
    (let [input "* Head"
          exp-out {:type :head
                   :val [1 [:text Head]]
                   :children []}
          actual-out (-> input
                         tok/tokenize
                         parser/parse)]
      (is (= exp-out actual-out)))))
