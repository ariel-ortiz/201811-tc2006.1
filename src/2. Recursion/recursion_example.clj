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
