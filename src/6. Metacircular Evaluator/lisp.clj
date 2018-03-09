
; Helper functions

(defn third
  "Returns third element of lst."
  [lst]
  (nth lst 2))

(defn fourth
  "Returns fourth element of lst."
  [lst]
  (nth lst 3))

(defn evaluate
  "Evaluate an expression expr in the context of
  an environment env and return the result."
  [expr env]
  (cond

    ; 1- Variable references
    (symbol? expr)
    (if (contains? env expr)
      (get env expr)
      (throw (RuntimeException.
               (str "Unbound variable: " expr))))

    ; 2- Special forms
    ; 3- Functions and invoke
    (list? expr)
    (case (first expr)

      nil
      ()

      quote
      (second expr)

      if
      (if (evaluate (second expr) env)
        (evaluate (third expr) env)
        (evaluate (fourth expr) env)))

    ; 4- Everything else evaluates to itself
    :else
    expr))

