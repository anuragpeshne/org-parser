(ns org-parser.tokenizer-test
  (:require [org-parser.tokenizer :as tok]
            [clojure.test :refer :all]
            [clojure.string :as str]))

(deftest heading-test
  (testing "Simple h1"
    (let [input "* Head"
          exp-out (list [:head 1 [[:text "Head"]]])
          actual-out (tok/tokenize input)]
      (is (= exp-out actual-out))))
  (testing "Simple h2"
    (let [input "** Head"
          exp-out (list [:head 2 [[:text "Head"]]])
          actual-out (tok/tokenize input)]
      (is (= exp-out actual-out))))
  (testing "Nested headings"
    (let [input (str/trim "
* Head1 ~code~
** Head2")
          exp-out (list [:head 1 [[:text "Head1 "] [:inline-code "code"]]]
                        [:head 2 [[:text "Head2"]]])
          actual-out (tok/tokenize input)]
      (is (= exp-out actual-out)))))

(deftest org-directive-test
  (testing "code block"
    (let [input (str/trim "
#+BEGIN_SRC c
int a = 0;
int b = a;
#+END_SRC")
          expected-out (list [:code-block " c" ["int a = 0;" "int b = a;"]])
          actual-out (tok/tokenize input)]
      (is (= expected-out actual-out))))
  (testing "incomplete code block"
    (let [input (str/trim "
#+BEGIN_SRC c
int a = 0;
int b = a;")
          expected-out (list [:paragraph [[:text "#+BEGIN_SRC c"]]]
                             [:paragraph [[:text "int a = 0;"]]]
                             [:paragraph [[:text "int b = a;"]]])
          actual-out (tok/tokenize input)]
      (is (= expected-out actual-out))))
  (testing "example block"
    (let [input-content (str/trim "
A very good example
With multiple lines")
          input (str/join "\n" (list "#+BEGIN_EXAMPLE"
                                     input-content
                                     "#+END_EXAMPLE"))
          expected-out (list [:example-block "" (str/split-lines input-content)])
          actual-out (tok/tokenize input)]
      (is (= expected-out actual-out))))
  (testing "example verse"
    (let [input-content (str/trim "
A very good verse
With multiple lines")
          input (str/join "\n" (list "#+BEGIN_VERSE"
                                     input-content
                                     "#+END_VERSE"))
          expected-out (list [:verse-block "" (str/split-lines input-content)])
          actual-out (tok/tokenize input)]
      (is (= expected-out actual-out)))))

(deftest inline-formatting-test
  (testing "inline bold formatting"
    (let [input "this is *important*"
          expected-out (list [:paragraph [[:text "this is "] [:bold "important"]]])
          actual-out (tok/tokenize input)]
      (is (= expected-out actual-out))))
  (testing "inline underline formatting"
    (let [input "this is _important_"
          expected-out (list [:paragraph [[:text "this is "] [:underline "important"]]])
          actual-out (tok/tokenize input)]
      (is (= expected-out actual-out))))
  (testing "inline italic formatting"
    (let [input "this is /important/"
          expected-out (list [:paragraph [[:text "this is "] [:italic "important"]]])
          actual-out (tok/tokenize input)]
      (is (= expected-out actual-out))))
  (testing "inline verbatim formatting"
    (let [input "this is =important="
          expected-out (list [:paragraph [[:text "this is "] [:verbatim "important"]]])
          actual-out (tok/tokenize input)]
      (is (= expected-out actual-out))))
  (testing "inline strikethrough formatting"
    (let [input "this is +important+"
          expected-out (list [:paragraph [[:text "this is "] [:strikethrough "important"]]])
          actual-out (tok/tokenize input)]
      (is (= expected-out actual-out)))))

(deftest list-test
  (testing "simple unordered list"
    (let [input (str/trim "
- an *important* list
  - with a sublist
")
          expected-out (list [:ulist 0 "-" [[:text "an "] [:bold "important"] [:text " list"]]]
                             [:ulist 2 "-" [[:text "with a sublist"]]])
          actual-out (tok/tokenize input)]
      (is (= expected-out actual-out))))
  (testing "simple ordered list"
    (let [input (str/trim "
1. an *important* list
   1) with a sublist
")
          expected-out (list [:olist 0 "1." [[:text "an "] [:bold "important"] [:text " list"]]]
                             [:olist 3 "1)" [[:text "with a sublist"]]])
          actual-out (tok/tokenize input)]
      (is (= expected-out actual-out)))))
