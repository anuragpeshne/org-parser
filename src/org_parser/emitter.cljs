(ns org-parser.compiler)

(defn to-html
  [ast]
  (-> js/document
      (.createElement "h1")
      (.-innerHTML)
      (set! "hello from emitter")))

(to-html)
(println "hello world")
