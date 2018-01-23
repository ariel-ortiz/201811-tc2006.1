(defn !
  "Computes and returns the factorial of n."
  [n]
  (if (zero? n)
    1
    (*' n (! (dec n)))))

(defn dup
  "Returns a new list where every element
  of lst is duplicated."
  [lst]
  (if (empty? lst)
    ()
    (cons (first lst)
          (cons (first lst)
                (dup (rest lst))))))

(defn pow
  [base expo]
  "Raises base to the power expo."
  (if (zero? expo)
    1
    (*' base (pow base (dec expo)))))

(defn countdown
  "Return a list with (n n-1 n-2 ... 1)."
  [n]
  (if (zero? n)
    ()
    (cons n (countdown (dec n)))))

(defn log2
  "Returns the logarithm base 2 of n, rounded to the
  closest integer that is less or equal to the real
  result."
  [n]
  (if (= n 1)
    0
    (inc (log2 (quot n 2)))))

(defn howmany
  "Returns how many times x appears in lst, which might
  contain nested lists."
  [x lst]
  (cond
    (empty? lst)
    0

    (list? (first lst))
    (+ (howmany x (first lst))
       (howmany x (rest lst)))

    (= x (first lst))
    (inc (howmany x (rest lst)))

    :else
    (howmany x (rest lst))))
