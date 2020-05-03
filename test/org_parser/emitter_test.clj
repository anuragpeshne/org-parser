(ns org-parser.emitter-test
  (:require [org-parser.emitter :as sut]
            [clojure.test :refer :all]
            [org-parser.parser :as parser]
            [org-parser.emitter :as emitter]
            [org-parser.tokenizer :as tok]))

(deftest heading-test
  (testing "simple h1"
    (let [input "* head1"
          exp-out "<h1>head1</h1>"
          actual-out (-> input
                         tok/tokenize
                         parser/parse
                         emitter/to-html)]
      (is (= exp-out actual-out)))))
