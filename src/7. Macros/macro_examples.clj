(defmacro debug
  "Debug macro. Evaluates and prints the result
  of an expression."
  [expr]
  (list 'let ['result expr]
        (list 'printf "debug %s => %s%n"
              (list 'quote expr)
              'result)
        'result))