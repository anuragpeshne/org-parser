(ns org-parser.parser-test
  (:require [clojure.test :refer :all]
            [org-parser.parser :as parser]
            [org-parser.tokenizer :as tok])
  (:import [org_parser.parser ASTNode]))

(deftest heading-test
  (testing "Simple h1"
    (let [input "* Head"
          exp-out (parser/ASTNode.
                   :root
                   nil
                   [(parser/ASTNode.
                     :head
                     [1 [[:text "Head"]]]
                     [])])
          actual-out (-> input
                         tok/tokenize
                         parser/parse)]
      (is (= exp-out actual-out)))))
