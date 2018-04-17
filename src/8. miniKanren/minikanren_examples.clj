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
  "Logical function that succeeds if every element in lst
  is duplicated in result."
  [lst result]
  (conde
    [(=== lst [])
     (=== result [])]
    [(fresh [head tail temp]
       (conso head tail lst)
       (dupo tail temp)
       (appendo [head head] temp result))]))

(defn reverseo
  "Logical function that succeeds if the reverse of lst
  is result."
  [lst result]
  (conde
    [(=== lst [])
     (=== result [])]
    [(fresh [head tail temp]
        (reverseo tail temp)
        (conso head tail lst)
        (appendo temp [head] result))]))

(defn twin-dupleo
  "Logical function that succeeds if lst is a two element
  sequence where both elements are the same."
  [lst]
  (conde
    [(fresh [h1 t1]
       (conso h1 t1 lst)
       (conso h1 [] t1))]))
