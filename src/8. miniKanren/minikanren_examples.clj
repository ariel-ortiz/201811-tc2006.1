(use '[clojure.core.logic :rename {== ===}])

(defn lasto
  "Logical function that succeed if the last element
  of lst is result."
  [lst result]
  (conde
    [(=== lst [result])]
    [(fresh [head tail]
        (conso head tail lst)
        (lasto tail result))]))

(defn dupo
  "Logical function that succeeds if every element in list
  is duplicated in result."
  [lst result]
  (conde
    [(=== lst [])
     (=== result [])]
    [(fresh [head tail temp]
       (conso head tail lst)
       (dupo tail temp)
       (appendo [head head] temp result))]))