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

(defn- collect-listitem-into-list
  [ast-list]
  (loop [[current-node & rest-node-list] ast-list
         processed-list []
         current-parent-node nil]
    (if (not (nil? current-node))
      (let [node-type (:type current-node)
            [to-conj-nodes parent]
            (if (or (= :ulist node-type)
                    (= :olist node-type))
              (let [parent-type (keyword (str (name node-type) "-parent"))]
                (if (and (not (nil? current-parent-node))
                         (= parent-type (.type current-parent-node)))
                  (let [children (:children current-parent-node)]
                    [[] (assoc current-parent-node :children (conj children current-node))])
                  [[current-parent-node] (ASTNode. parent-type nil [current-node])]))
              (if (nil? current-parent-node)
                [[current-node] nil]
                [[current-parent-node current-node] nil]))
            [non-nil-to-conj-nodes parent] [(filter #(not (nil? %1)) to-conj-nodes) parent]]
        (recur rest-node-list (concat processed-list non-nil-to-conj-nodes) parent))
      (if (nil? current-parent-node)
        processed-list
        (conj processed-list current-parent-node)))))

(defn- post-process-children
  [ast-nodes]
  (let [processed-node-list (collect-listitem-into-list ast-nodes)]
    processed-node-list))

(defn parse
  ([tokens]
   (let [root-terminating-condition (fn [_] false)
         [child-nodes _] (parse tokens root-terminating-condition)
         root-node (ASTNode. :root nil child-nodes)]
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
                           [child-nodes returned-unprocessed-tokens]
                           (parse
                            rest-tokens
                            (get-heading-terminating-condition current-heading-level))]
                       [(ASTNode. :head rest-token-content child-nodes)
                        returned-unprocessed-tokens])
               :paragraph (let [[child-nodes returned-unprocessed-tokens]
                                (loop [loop-unprocessed-tokens rest-tokens
                                       sibling-paragraph-nodes [(second current-token)]]
                                  (let [[loop-current-token & loop-rest-tokens] loop-unprocessed-tokens
                                        [loop-current-token-type loop-current-token-content & _] loop-current-token]
                                    (if (= loop-current-token-type :paragraph)
                                      (recur loop-rest-tokens (conj sibling-paragraph-nodes loop-current-token-content))
                                      [sibling-paragraph-nodes loop-unprocessed-tokens])))]
                           [(ASTNode. :paragraph child-nodes [])
                            returned-unprocessed-tokens])
               (:ulist :olist) (let [[list-type indentation list-symbol list-content & _] current-token
                                     [child-nodes returned-unprocessed-tokens]
                                     (parse
                                      rest-tokens
                                      (get-list-terminating-condition
                                       list-type
                                       list-symbol
                                       indentation))]
                                 [(ASTNode. list-type list-content child-nodes)
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
       [(post-process-children acc-children) unprocessed-tokens]))))
