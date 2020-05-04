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

(defn- map-with-side-effects
  [f target coll]
  (loop [current-target target
         [current-element & rest-elements] coll]
    (if (nil? current-element)
      current-target
      (let [new-target (f current-target current-element)]
        (recur new-target rest-elements)))))

(defn to-html
  [ast]
  (let [{node-type :type
         node-val :val
         node-children :children} ast]
    (case node-type
      :root (let [root-div (html-create-element "div"  nil)
                  children-html (map to-html node-children)]
              (doseq [child children-html] (html-append-child root-div child))
              root-div)
      :head (let [level (first node-val)
                  content (second node-val)
                  html-content (inline-format-to-html content)
                  h-tag (html-create-element (str "h" level)  nil)]
              (doseq [tag html-content] (html-append-child h-tag tag))
              h-tag)
      :paragraph (let [content (map inline-format-to-html node-val)
                       p-tag (html-create-element "p" nil)]
                   (println content)
                   (doseq [tag content] (html-append-child p-tag tag))
                   p-tag)
      (throw-unimplemented-exception node-type))))
