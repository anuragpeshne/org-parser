(ns org-parser.tokenizer-test
  (:require [org-parser.tokenizer :as tok]
            [clojure.test :refer :all]
            [clojure.string :as str]))

(deftest heading-test
  (testing "Simple h1"
    (let [input "* Head"
          exp-out (list [:head1 [[:text "Head"]]])
          actual-out (tok/tokenize input)]
      (is (= exp-out actual-out))))
  (testing "Simple h2"
    (let [input "** Head"
          exp-out (list [:head2 [[:text "Head"]]])
          actual-out (tok/tokenize input)]
      (is (= exp-out actual-out))))
  (testing "Nested headings"
    (let [input (str/trim "
* Head1 ~code~
** Head2")
          exp-out (list [:head1 [[:text "Head1 "] [:inline-code "~code~"]]] [:head2 [[:text "Head2"]]])
          actual-out (tok/tokenize input)]
      (is (= exp-out actual-out)))))

(deftest org-directive-test
  (testing "code block"
    (let [input (str/trim "
#+BEGIN_SRC c
int a = 0;
int b = a;
#+END_SRC")
          expected-out (list [:code-block ["int a = 0;" "int b = a;"] " c"])
          actual-out (tok/tokenize input)]
      (is (= expected-out actual-out))))
  (testing "incomplete code block"
    (let [input (str/trim "
#+BEGIN_SRC c
int a = 0;
int b = a;")
          expected-out (list [[:text "#+BEGIN_SRC c"]]
                             [[:text "int a = 0;"]]
                             [[:text "int b = a;"]])
          actual-out (tok/tokenize input)]
      (is (= expected-out actual-out))))
  (testing "example block"
    (let [input-content (str/trim "
A very good example
With multiple lines")
          input (str/join "\n" (list "#+BEGIN_EXAMPLE"
                                     input-content
                                     "#+END_EXAMPLE"))
          expected-out (list [:example (str/split-lines input-content)])
          actual-out (tok/tokenize input)]
      (is (= expected-out actual-out))))
  (testing "example verse"
    (let [input-content (str/trim "
A very good verse
With multiple lines")
          input (str/join "\n" (list "#+BEGIN_VERSE"
                                     input-content
                                     "#+END_VERSE"))
          expected-out (list [:verse (str/split-lines input-content)])
          actual-out (tok/tokenize input)]
      (is (= expected-out actual-out))))
  (testing "inline bold formatting"
    (let [input "this is *important*"
          expected-out (list [[:text "this is "] [:bold "*important*"]])
          actual-out (tok/tokenize input)]
      (is (= expected-out actual-out))))
  (testing "inline underline formatting"
    (let [input "this is _important_"
          expected-out (list [[:text "this is "] [:underline "_important_"]])
          actual-out (tok/tokenize input)]
      (is (= expected-out actual-out))))
  (testing "inline italic formatting"
    (let [input "this is /important/"
          expected-out (list [[:text "this is "] [:italic "/important/"]])
          actual-out (tok/tokenize input)]
      (is (= expected-out actual-out))))
  (testing "inline verbatim formatting"
    (let [input "this is =important="
          expected-out (list [[:text "this is "] [:verbatim "=important="]])
          actual-out (tok/tokenize input)]
      (is (= expected-out actual-out))))
  (testing "inline strikethrough formatting"
    (let [input "this is +important+"
          expected-out (list [[:text "this is "] [:strikethrough "+important+"]])
          actual-out (tok/tokenize input)]
      (is (= expected-out actual-out)))))
