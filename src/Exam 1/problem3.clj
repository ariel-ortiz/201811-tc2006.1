;==========================================================
; Type your student ID and name here.
;==========================================================

(use 'clojure.test)

;==========================================================
(defn thingy
  "Returns a new list with n elements, where the first
  element is x, the second element is f(x), the third
  element is f(f(x)), and so on."
  [n f x]
  ())

;==========================================================
(deftest test-thingy
  (is (= '() (thingy 0 dec 0)))
  (is (= '(1 2 3 4) (thingy 4 inc 1)))
  (is (= '(1 2 4 8 16 32 64 128 256 512)
         (thingy 10 (fn [n] (* n 2)) 1)))
  (is (= '(1 10 100 1000 10000 100000)
         (thingy 6 (fn [n] (* n 10)) 1)))
  (is (= '(() (*) (* *) (* * *) (* * * *))
         (thingy 5 (fn [n] (cons '* n)) ()))))

;==========================================================
(run-tests)
