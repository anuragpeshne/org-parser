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
   (let [root-terminating-condition #(true)
         [children-node _] (parse tokens root-terminating-condition)
         root-node (ASTNode. :root nil children-node)]
     root-node))
  ([tokens terminating-condition]
   (loop [acc-children []
          unprocessed-tokens tokens]
     (if (seq unprocessed-tokens)
       (let [[current-token & rest-tokens] tokens
             [token-type & token-content] current-token
             [returned-node returned-unprocessed-tokens]
             (case token-type
               :head (let [current-heading-level (second current-token)
                           [children-node returned-unprocessed-tokens]
                           (parse
                            rest-tokens
                            (get-heading-terminating-condition current-heading-level))]
                       [(ASTNode. :head token-content children-node)
                        returned-unprocessed-tokens])
               (throw (str "unknown token: " token-type)))]
         (recur (conj acc-children returned-node) returned-unprocessed-tokens))
       [acc-children unprocessed-tokens]))))
