(ns org-parser.emitter)

#? (:clj (println "hello from clj")
    :cljs (js/alert "hello from cljs"))

(defn- create-html-element
  [element-type inner-html propertities]
  #?(:cljs (-> (-> js/document
                   (.createElement element-type)
                   (.-innerHTML)
                   (set! inner-html)))))

(defn to-html
  [ast]
  (create-html-element "h1" "hello from emitter" nil))
