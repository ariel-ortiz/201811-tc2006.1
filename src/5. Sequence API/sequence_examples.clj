(defn add-list
  "Adds all the numbers in lst."
  [lst]
  (reduce + 0 lst))

(defn list-of-symbols?
  "Returns true if all elements (possibly zero) in lst are
  symbols, false otherwise."
  [lst]
  (every? symbol? lst))

(defn invert-pairs
  "Takes as an argument a list of vectors containing two
  elements each. It returns a new list with every vector
  pair inverted."
  [lst]
  (map #(vec (reverse %)) lst))

(defn enlist
  "Surrounds in a list every upper-level element of the
   list it takes as input."
  [lst]
  (map list lst))

(defn insert
  "Returns a list where x in inserted in order inside lst."
  [x lst]
  (concat
    (take-while #(< % x) lst)
    (list x)
    (drop-while #(< % x) lst)))

(defn binary
  "Returns a list of binary digits representing the converted
  value of n."
  [n]
  (second
    (first
      (drop-while
        (fn [[n lst]] (not= n 0))
        (iterate (fn [[n lst]]
                   [(quot n 2) (cons (rem n 2) lst)])
                 [n ()])))))