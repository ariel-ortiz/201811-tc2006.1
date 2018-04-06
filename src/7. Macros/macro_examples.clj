(defmacro debug
  "Debug macro. Evaluates and prints the result
  of an expression."
  [expr]
  `(let [result# ~expr]
     (printf "debug %s => %s%n"
             (quote ~expr)
             result#)
     result#))