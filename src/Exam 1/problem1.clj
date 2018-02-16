;==========================================================
; Type your student ID and name here.
;==========================================================

(use 'clojure.test)

;==========================================================
(defn list-of-lists
  "Returns a new list containing n empty lists."
  [n]
  ())

;==========================================================
(deftest test-list-of-lists
  (is (= () (list-of-lists 0)))
  (is (= '(()) (list-of-lists 1)))
  (is (= '(() () ())
         (list-of-lists 3)))
  (is (= '(() () () () () () () () () ())
         (list-of-lists 10)))
  (is (= '(() () () () () () () () () ()
            () () () () () () () () () ()
            () () () () () () () () () ()
            () () () () () () () () () ()
            () () () () () () () () () ()
            () () () () () () () () () ()
            () () () () () () () () () ()
            () () () () () () () () () ()
            () () () () () () () () () ()
            () () () () () () () () () ())
         (list-of-lists 100))))

;==========================================================
(run-tests)
