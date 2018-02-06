(defn my-comp
  "Our implementation of the composite function of f and g."
  [f g]
  (fn [x]
    (f (g x))))

(defn f1 [x] (+ (* 3 x) 1))

(defn f2 [x] (* x x))

(def f3 (my-comp f1 f2))

(def f4 (my-comp f2 f1))

(def f5 (my-comp f3 f4))

(defn f
  [a b c d]
  (* (+ a b) (+ c d)))

(defn f-curry [a]
  (fn [b]
    (fn [c]
      (fn [d]
        (* (+ a b) (+ c d))))))

(defn my-map
  "Returns a new list comprised of the same elements
  in lst but applying fun to them."
  [fun lst]
  (if (empty? lst)
    ()
    (cons (fun (first lst))
          (my-map fun (rest lst)))))