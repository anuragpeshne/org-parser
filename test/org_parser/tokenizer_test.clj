(ns org-parser.tokenizer-test
  (:require [org-parser.tokenizer :as tok]
            [clojure.test :refer :all]))

(deftest heading-test
  (testing "Simple h1"
    (let [input "* Head"
          exp-out (list [:head1 [:text "Head"]])
          actual-out (tok/tokenize input)]
      (is (= exp-out actual-out))))
  (testing "Simple h2"
    (let [input "** Head"
          exp-out (list [:head2 [:text "Head"]])
          actual-out (tok/tokenize input)]
      (is (= exp-out actual-out)))))
