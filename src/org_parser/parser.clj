(ns org-parser.parser)

(defn- parse-heading
  [heading-type content parsed-ast WIP-node]
  ; TODO add as a child node or attach the WIP node and create a new node)

(defn parse
  [tokens]
  (loop [input-tokens tokens
         WIP-node []
         parsed-ast []]
    (when (seq input-tokens)
      (let [[token-type & token-content] (first input-tokens)
            [returned-ast returned-node]
            (case token-type
              (:head1 :head2 :head3 :head4 :head5 :head6)
              (parse-heading token-type token-content parsed-ast WIP-node))]
        (recur (rest input-tokens) returned-node returned-ast)))))
