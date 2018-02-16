;==========================================================
; Type your student ID and name here.
;==========================================================

(use 'clojure.test)

;==========================================================
(defn proper-divisor-sum
  "Returns the sum of all proper divisors of n."
  [n]
  0)

;==========================================================
(deftest test-proper-divisor-sum
  (is (= 8 (proper-divisor-sum 10)))
  (is (= 22 (proper-divisor-sum 20)))
  (is (= 28 (proper-divisor-sum 28)))
  (is (= 122072 (proper-divisor-sum 104728)))
  (is (= 1 (proper-divisor-sum 104729))))

;==========================================================
(run-tests)
