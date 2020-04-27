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
               (throw (Exception. (str "unknown token: " token-type))))]
         (recur (conj acc-children returned-node) returned-unprocessed-tokens))
       [acc-children unprocessed-tokens]))))
