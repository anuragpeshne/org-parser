(ns org-parser.emitter)

(defn- create-html-element
  [element-type inner-html propertities]
  #?(:cljs (let [element (-> js/document
                             (.createElement element-type))]
                 (-> element
                     (.-innerHTML)
                     (set! inner-html))
                 element)))

(defn to-html
  [ast]
  (create-html-element "h1" "hello from emitter" nil))
