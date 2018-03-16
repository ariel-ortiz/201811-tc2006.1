(import 'clojure.lang.IFn)
(declare evaluate)

(deftype Closure
  [env params body]
  IFn
  (invoke
    [self args]
    (evaluate body
              (merge @env (zipmap params args))))
  (applyTo
    [self args]
    (self args)))

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
        (evaluate (fourth expr) env))

      lambda
      (->Closure (atom env) (second expr) (third expr))

      label
      (let [lambda-expr (evaluate (third expr) env)]
        (swap! (.env lambda-expr)
               #(assoc % (second expr) lambda-expr))
        lambda-expr)

      ; else, function invocation
      (apply (evaluate (first expr) env)
             (map #(evaluate % env)
                  (rest expr))))

    ; 4- Everything else evaluates to itself
    :else
    expr))

(evaluate
  '((lambda (f x) (f (f x)))
    (lambda (x) (* x 2))
    10)
  {'* *})

; => 40

(evaluate
  '((label dup (lambda (lst)
                 (if (eq lst ())
                   ()
                   (cons (car lst)
                         (cons (car lst)
                               (dup (cdr lst)))))))
    (quote (a b c)))
  {'eq   =
   'cons cons
   'car  first
   'cdr  rest})

;=> (a a b b c c)