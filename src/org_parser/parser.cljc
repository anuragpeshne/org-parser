(ns org-parser.parser)

(defrecord ASTNode [type val children])

(defn- get-heading-terminating-condition
  [current-heading-level]
  (fn
    [token]
    (if (and (= (first token) :head)
             (<= (second token) current-heading-level))
      true
      false)))

(defn- get-list-terminating-condition
  [list-type list-symbol indentation]
  (fn
    [token]
    (let [[token-type token-indentation token-list-type & _] token]
      (if (or (= token-type :ulist)
              (= token-type :olist))
        (if (and (= token-type list-type)
                 (= token-list-type list-symbol)
                 (> token-indentation indentation))
          false
          true)
        false))))

(defn parse
  ([tokens]
   (let [root-terminating-condition (fn [_] false)
         [children-node _] (parse tokens root-terminating-condition)
         root-node (ASTNode. :root nil children-node)]
     root-node))
  ([tokens should-terminate?]
   (loop [acc-children []
          unprocessed-tokens tokens]
     (if (and (seq unprocessed-tokens)
              (not (should-terminate? (first unprocessed-tokens))))
       (let [[current-token & rest-tokens] unprocessed-tokens
             [token-type & rest-token-content] current-token
             [returned-node returned-unprocessed-tokens]
             (case token-type
               :head (let [current-heading-level (second current-token)
                           [children-node returned-unprocessed-tokens]
                           (parse
                            rest-tokens
                            (get-heading-terminating-condition current-heading-level))]
                       [(ASTNode. :head rest-token-content children-node)
                        returned-unprocessed-tokens])
               :paragraph [(ASTNode.
                            :paragraph
                            (first rest-token-content)
                            [])
                           rest-tokens]
               (:ulist :olist) (let [[list-type indentation list-symbol list-content & _]
                                     current-token
                                     [children-node returned-unprocessed-tokens]
                                     (parse
                                      rest-tokens
                                      (get-list-terminating-condition
                                       list-type
                                       list-symbol
                                       indentation))]
                                 [(ASTNode. list-type list-content children-node)
                                  returned-unprocessed-tokens])
               (:code-block :verse-block :example-block)
               (let [[_ options token-value] current-token]
                 [(ASTNode.
                   token-type
                   [options token-value]
                   [])
                  rest-tokens])
               [(ASTNode. :paragraph current-token []) rest-tokens])]
         (recur (conj acc-children returned-node) returned-unprocessed-tokens))
       [acc-children unprocessed-tokens]))))
