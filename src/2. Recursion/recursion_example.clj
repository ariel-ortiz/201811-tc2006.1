(defn !
  "Computes and returns the factorial of n. Uses
  explicit recursion."
  [n]
  (if (zero? n)
    1
    (*' n (! (dec n)))))

(defn !-loop
  "Computes and returns the factorial of n. Uses
  loop/recur."
  [n]
  (loop [index   1
         result  1]
    (if (> index n)
      result
      (recur (inc index) (*' index result)))))


(defn dup
  "Returns a new list where every element
  of lst is duplicated. Uses explicit recursion."
  [lst]
  (if (empty? lst)
    ()
    (cons (first lst)
          (cons (first lst)
                (dup (rest lst))))))

(defn dup-loop
  "Returns a new list where every element
  of lst is duplicated. Uses loop/recur."
  [lst]
  (loop [lst lst
         result ()]
    (if (empty? lst)
      (reverse result)
      (recur (rest lst)
             (cons (first lst)
                   (cons (first lst) result))))))

(defn pow
  [base expo]
  "Raises base to the power expo. Uses explicit recursion."
  (if (zero? expo)
    1
    (*' base (pow base (dec expo)))))

(defn pow-loop
  "Raises base to the power expo. Uses loop/recur."
  [base expo]
  (loop [result 1
         index  0]
    (if (= index expo)
      result
      (recur (*' result base) (inc index)))))

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

(defn fibo
  "Returns the n-th element of the Fibonacci sequence.
  Uses explicit recursion."
  [n]
  (if (< n 2)
    n
    (+ (fibo (- n 1)) (fibo (- n 2)))))

(defn fibo-loop
  "Returns the n-th element of the Fibonacci sequence.
  Uses loop/recur."
  [n]
  (loop [a     0
         b     1
         index 0]
    (if (= index n)
      a
      (recur b (+' a b) (inc index)))))