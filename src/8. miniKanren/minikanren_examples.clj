(use '[clojure.core.logic :rename {== ===}])

(defne lasto
  "Logical function that succeed if the last element
  of lst is result."
  [lst result]
  ([[result] result])
  ([[head . tail] result]
   (lasto tail result)))

(defne dupo
  "Logical function that succeeds if every element in lst
  is duplicated in result."
  [lst result]
  ([[] []])
  ([[head . tail] [head head . temp]]
   (dupo tail temp)))

(defne reverseo
  "Logical function that succeeds if the reverse of lst
  is result."
  [lst result]
  ([[] []])
  ([[head . tail] result]
   (fresh [temp]
     (reverseo tail temp)
     (appendo temp [head] result))))

(defne twin-dupleo
  "Logical function that succeeds if lst is a two element
  sequence where both elements are the same."
  [lst]
  ([[x x]]))

(defne listso
  "Logical function that succeeds if all the elements of
  lst are contained within a list in result."
  [lst result]
  ([[] []])
  ([[head . tail] [[head] . temp]]
   (listso tail temp)))
