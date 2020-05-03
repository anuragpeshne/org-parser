(ns org-parser.emitter-test
  (:require [org-parser.emitter :as sut]
            [clojure.test :refer :all]
            [org-parser.parser :as parser]
            [org-parser.emitter :as emitter]
            [org-parser.tokenizer :as tok]
            [clojure.string :as str]))

(defn- check-html-tag
  [html-node exp-tag]
  (let [actual-tag (re-matches #"^<(.*).*>?.*" html-node)]
    (is (= actual-tag exp-tag))))

(defn- check-html-class
  [html-node exp-class-list]
  (let [actual-class-str (re-matches #"^<.* class=\"(.*)\"?.*>.*" html-node)
        sorted-class-list (-> actual-class-str
                              str/split #","
                              sort)
        sorted-exp-class-list (sort exp-class-list)]
    (is (= (sorted-exp-class-list sorted-class-list)))))

(defn- check-html-content
  [html-node exp-content]
  (let [actual-content (re-matches #"^<.*>?(.*)</.*>")]
    (is (= exp-content actual-content))))

(deftest heading-test
  (testing "simple h1"
    (let [input "* head1"
          exp-out "<h1>head1</h1>"
          actual-out (-> input
                         tok/tokenize
                         parser/parse
                         emitter/to-html)]
      (is (= exp-out actual-out)))))

(deftest text-test
  (testing "simple multiline text with inline formatting"
    (let [input (str/trim "
The first line in the /multiline text/
is actually *two* lines.
")
          exp-out (str/trim "
<p>The first line in the <span class=\"org-italic\">multiline text</span> is actually <span class=\"org-bold\">two</span> lines.</p>
")
          actual-out (-> input
                         tok/tokenize
                         parser/parse
                         emitter/to-html)]
      (is (= exp-out actual-out)))))
