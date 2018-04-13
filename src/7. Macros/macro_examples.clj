(defmacro debug
  "Debug macro. Evaluates and prints the result
  of an expression."
  [expr]
  `(let [result# ~expr]
     (printf "debug %s => %s%n"
             (quote ~expr)
             result#)
     result#))

;;;; Bad code, because both arguments a and b are
;;;; evaluated before executing the function's body.
;(defn my-and
;  [a b]
;  (if a
;    b
;    false))

(defmacro my-and
  "Implements our own version of the and macro."
  ([] true)
  ([a] a)
  ([a & b]
   `(let [temp# ~a]
      (if temp#
        (my-and ~@b)
        temp#))))

(defmacro my-comment
  "Ignores body and returns nil."
  [& body]
  nil)
