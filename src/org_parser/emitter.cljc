(ns org-parser.emitter
  (:require [clojure.string :as str]))

(defn- throw-unimplemented-exception
  [name]
  (throw #?(:clj (Exception. (str "Unimplemented: " name))
            :cljs (js/Error. (str "Unimplemented: " name)))))

(defn- create-html-element
  [element-type inner-html propertities]
  #?(:cljs (let [element (-> js/document
                             (.createElement element-type))]
                 (-> element
                     (.-innerHTML)
                     (set! inner-html))
                 element)
     :clj (let [property-string (if (nil? propertities)
                                  ""
                                  (str " " propertities))]
            (str "<" element-type property-string ">" inner-html "</" element-type ">"))))

(defn- inline-format-element-to-html
  [element]
  (let [[format content] element]
    (case format
      :text content
      :bold (create-html-element "span" content "class=\"org-bold\"")
      :italic (create-html-element "span" content "class=\"org-italic\"")
      (throw-unimplemented-exception format))))

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
      :paragraph (create-html-element
                  "p"
                  (str/join " " (map inline-format-to-html node-val))
                  nil)
      (throw-unimplemented-exception node-type))))
