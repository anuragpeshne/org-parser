(ns org-parser.parser)

(defn- parse-heading
  [heading-type content parsed-ast WIP-node]
                                        ; TODO add as a child node or attach the WIP node and create a new node)
  )

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
         root-node (ASTNode. (:type :root) (:val nil) (:children children-node))]
     root-node))
  ([tokens terminating-condition]
   (loop [acc-children []
          unprocessed-token tokens]
     (when (seq unprocessed-tokens)
       (let [[current-token & rest-tokens] tokens
             [token-type & token-content] current-token
             [returned-node returned-unprocessed-tokens]
             (case token-type
               :head (let [current-heading-level (second current-token)]
                       (parse
                        rest-tokens
                        (get-heading-terminating-condition current-heading-level))))]
         (recur (conj acc-children returned-node) returned-unprocessed-tokens))))))
