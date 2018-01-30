(declare variable? same-variable? sum?
         addend augend make-sum product?
         multiplier multiplicand make-product)

(defn deriv
  "Takes an algebraic expression exp and a variable
  var, and returns the derivate of the expression with
  respect to the variable."
  [exp var]
  (cond

    (number? exp)
    0

    (variable? exp)
    (if (same-variable? exp var) 1 0)

    (sum? exp)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var))

    (product? exp)
    (make-sum (make-product (multiplier exp)
                            (deriv (multiplicand exp) var))
              (make-product (multiplicand exp)
                            (deriv (multiplier exp) var)))
    :else
    (throw (IllegalArgumentException.
             (str "unknown expression type: " exp)))))

(defn variable?
  "Returns true if x is a variable, false otherwise."
  [x]
  (symbol? x))

(defn same-variable?
  "Returns true if v1 and v2 are the same variable, false otherwise."
  [v1 v2]
  (and (variable? v1) (variable? v2) (= v1 v2)))

(defn make-sum
  "Returns a representation of the sum of a1 and a2."
  [a1 a2]
  (list '+ a1 a2))

(defn make-product
  "Returns a representation of the product of m1 and m2."
  [m1 m2]
  (list '* m1 m2))

(defn sum?
  "Returns true if exp is a sum, false otherwise."
  [exp]
  (and (list? exp) (= (first exp) '+)))

(defn product?
  "Returns true if exp is a product, false otherwise."
  [exp]
  (and (list? exp) (= (first exp) '*)))

(defn addend
  "Returns the addend (addition's first operand) of exp."
  [exp]
  (nth exp 1))

(defn augend
  "Returns the augend (addition's second operand) of exp."
  [exp]
  (nth exp 2))

(defn multiplier
  "Returns the multiplier (product's first operand) of exp."
  [exp]
  (nth exp 1))

(defn multiplicand
  "Returns the multiplicand (product's second operand) of exp."
  [exp]
  (nth exp 2))
