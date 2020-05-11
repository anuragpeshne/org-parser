(ns org-parser.emitter
  (:require [clojure.string :as str]))

(defn- throw-unimplemented-exception
  [name]
  (throw #?(:clj (Exception. (str "Unimplemented: " name))
            :cljs (js/Error. (str "Unimplemented: " name)))))

(defn html-append-child
  [parent child]
  #?(:cljs (.appendChild parent child)
     :clj (let [[left right] (str/split parent "><")]
            (str left ">" child "<" right))))

(defn set-inner-html
  [parent content]
  #?(:cljs (do (-> parent
                   (.-innerHTML)
                   (set! content))
               parent)
     :clj (let [[left right] (str/split parent #"><")]
            (str left ">" content "<" right))))

(defn html-create-element
  [element-type propertities]
  #?(:cljs (let [element (.createElement js/document element-type)]
             (do
               (doseq [[key prop] propertities] (.setAttribute element key prop))
               element))
     :clj (let [property-string (if (nil? propertities)
                                  ""
                                  (str " " propertities))]
            (str "<" element-type property-string "></" element-type ">"))))

(defn- inline-format-element-to-html
  [element]
  (let [[format content] element]
    (case format
      :text (-> (html-create-element "span" {"class" "org-text"})
                (set-inner-html content))
      :bold (-> (html-create-element "span" {"class" "org-bold"})
                (set-inner-html content))
      :italic (-> (html-create-element "span" {"class" "org-italic"})
                  (set-inner-html content))
      (throw-unimplemented-exception format))))

(defn- inline-format-to-html
  [text-list]
  (map inline-format-element-to-html text-list))

  (defn- to-html-internal
    [ast]
    (println ast)
    (let [{node-type :type
           node-val :val
           node-children :children} ast
          children-html (reduce concat [] (map to-html-internal node-children))
          node-html (case node-type
                      :head (let [level (first node-val)
                                  content (second node-val)
                                  html-content (inline-format-to-html content)
                                  h-tag (html-create-element (str "h" level)  nil)]
                              (doseq [tag html-content] (html-append-child h-tag tag))
                              h-tag)
                      :paragraph (let [p-tag (html-create-element "p" nil)]
                                   (doseq [line node-val]
                                     (let [line-html (inline-format-to-html line)]
                                       (doseq [span-tag line-html] (html-append-child p-tag span-tag))))
                                   p-tag)
                      (:olist-parent :ulist-parent) (let [list-type (subs (name node-type) 0 2)
                                                          parent-tag (html-create-element list-type nil)
                                                          li-list-content (map to-html-internal node-val)]
                                                      (doseq [li-content li-list-content]
                                                        (doseq [tag li-content] (html-append-child parent-tag tag)))
                                                      parent-tag)
                      (:ulist :olist) (let [li-tag (html-create-element "li" nil)
                                            html-content (inline-format-to-html node-val)]
                                        (doseq [tag html-content] (html-append-child li-tag tag))
                                        li-tag)
                      (throw-unimplemented-exception node-type))
          result-html (conj children-html node-html)]
      result-html))

  (defn to-html
    [{node-type :type
      node-val :val
      node-children :children}]
    (if (= node-type :root)
      (let [root-div (html-create-element "div" nil)
            children-html (reduce concat [] (map to-html-internal node-children))]
        (doseq [child children-html] (html-append-child root-div child))
        root-div)
      (throw #?(:clj (Exception. "Root node of AST not of type root")
                :cljs (js/Error. "Root node of AST not of type root")))))
