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
  (->>
    [n ()]
    (iterate (fn [[n lst]]
               [(quot n 2) (cons (rem n 2) lst)]))
    (drop-while
      (fn [[n lst]] (not= n 0)))
    first
    second))

(defn prime-factors
  "Returns a list with all the prime factors of n."
  [n]
  (->>
    [n 2 ()]
    (iterate (fn [[n p lst]]
               (if (zero? (rem n p))
                 [(quot n p) p (cons p lst)]
                 [n (inc p) lst])))
    (drop-while
      (fn [[n p lst]] (not= n 1)))
    first
    rest
    rest
    first
    reverse))

(defn pack
  "Returns a list with consecutive equal elements packed into
  their own list."
  [lst]
  (partition-by identity lst))
