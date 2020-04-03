(ns org-parser.core-test
  (:require [clojure.test :refer :all]
            [org-parser.core :refer :all]))

(defn- wrap-html
  ([body] (wrap-html "<head></head>" body))
  ([head body] (str "<html>" head body "</html>")))

(defn- wrap-body
  [element]
  (wrap-html (str "<body>" element "</body>")))

(deftest heading-test
  (testing "Simple level 1 heading"
    (let [input "* Hello World"
          expected-output (wrap-body "<h1>Hello World</h1>")]
      (is (=  expected-output
              (org-parser.core/compile-to-html input))))))
