(ns org-parser.parser-test
  (:require [clojure.test :refer :all]
            [org-parser.parser :as parser]
            [org-parser.tokenizer :as tok]
            [clojure.string :as str])
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
      (is (= exp-out actual-out))))
  (testing "Complex heading test"
    (let [input (str/trim "
* Head
** Head 2
** Head 3
   Test content of a paragraph
* Head 4")
          exp-out (parser/ASTNode.
                   :root
                   nil
                   [(parser/ASTNode.
                     :head
                     [1 [[:text "Head"]]]
                     [(parser/ASTNode.
                       :head
                       [2 [[:text "Head 2"]]]
                       [])
                      (parser/ASTNode.
                       :head
                       [2 [[:text "Head 3"]]]
                       [(parser/ASTNode.
                           :paragraph
                           [[:text "   Test content of a paragraph"]]
                           [])])])
                    (parser/ASTNode.
                     :head
                     [1 [[:text "Head 4"]]]
                     [])])
          actual-out (-> input
                         tok/tokenize
                         parser/parse)]
      (is (= exp-out actual-out)))))
