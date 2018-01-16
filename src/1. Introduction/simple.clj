(use 'clojure.test)
(use 'clojure.math.numeric-tower)

(defn f2c
  "Converts f degrees Fahrenheit into Celsius."
  [f]
  (/ (* (- f 32) 5) 9))

(deftest test-f2c
  (is (= 100.0 (f2c 212.0)))
  (is (= 0.0 (f2c 32.0)))
  (is (= -40.0 (f2c -40.0))))

(defn sign
  "Returns -1 if n is negative, 0 if it's zero or
  1 if it's positive greater thanm zero."
  [n]
  (if (< n 0)
    -1
    (if (> n 0)
      1
      0)))

(deftest test-sign
  (is (= -1 (sign -5)))
  (is (= 1 (sign 10)))
  (is (= 0 (sign 0))))

(defn roots
  "Computes the roots of a quadratic equation with
  coefficients a, b, and c."
  [a b c]
  (let [d (- b)
        e (sqrt (- (* b b) (* 4 a c)))
        f (* 2 a)
        x1 (/ (+ d e) f)
        x2 (/ (- d e) f)]
    [x1 x2]))

(deftest test-roots
  (is (= [-1 -1] (roots 2 4 2)))
  (is (= [0 0] (roots 1 0 0)))
  (is (= [-1/4 -1] (roots 4 5 1))))

(defn bmi
  "Returns the body mass index (as a symbol) given
  the height a weight."
  [weight height]
  (let [BMI (/ weight (* height height))]
    (if (< BMI 20)
      'underweight
      (if (< BMI 25)
        'normal
        (if (< BMI 30)
          'obese1
          (if (< BMI 40)
            'obese2
            'obese3))))))

(deftest test-bmi
  (is (= 'underweight (bmi 45 1.7)))
  (is (= 'normal (bmi 55 1.5)))
  (is (= 'obese1 (bmi 76 1.7)))
  (is (= 'obese2 (bmi 81 1.6)))
  (is (= 'obese3 (bmi 120 1.6))))


(run-tests)