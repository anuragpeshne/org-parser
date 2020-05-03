(ns org-parser.emitter
  (:require [clojure.string :as str]))

(defn- throw-unimplement-exception
  []
  (throw #?(:clj (Exception. "Unimplemented node type")
            :cljs (js/Error. "Unimplemented node type"))))

(defn- create-html-element
  [element-type inner-html propertities]
  #?(:cljs (let [element (-> js/document
                             (.createElement element-type))]
                 (-> element
                     (.-innerHTML)
                     (set! inner-html))
                 element)
     :clj (str "<" element-type ">" inner-html "</" element-type ">")))

(defn- inline-format-element-to-html
  [element]
  (let [[format content] element]
    (case format
      :text content
      :bold (create-html-element "b" content nil)
      (throw-unimplement-exception))))

(defn- inline-format-to-html
  [text-list]
  (str/join (map inline-format-element-to-html text-list)))

(defn to-html
  [ast]
  (let [{node-type :type
         node-val :val
         node-children :children} ast]
    (case node-type
      :root (str/join "\n" (map to-html node-children))
      :head (let [level (first node-val)
                  content (second node-val)]
              (create-html-element (str "h" level) (inline-format-to-html content) nil))
      (throw-unimplement-exception))))
