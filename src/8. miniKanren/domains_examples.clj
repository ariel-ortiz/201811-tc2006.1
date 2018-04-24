(use '[clojure.core.logic :rename {== ===}])
(require '[clojure.core.logic.fd :as fd])

(defne addo
   "Logical function that succeeds if the sum of
   all elements in lst is equal to result."
  [lst result]
  ([[] 0])
  ([[head . tail] result]
   (fresh [temp]
     (addo tail temp)
     (fd/+ temp head result))))

(defn magic-square
  "Solves the 3x3 magic square puzzle."
  []
  (run*
    [q1 q2 q3
     q4 q5 q6
     q7 q8 q9]
    (fd/in q1 q2 q3 q4 q5 q6 q7 q8 q9 (fd/interval 1 9))
    (distincto [q1 q2 q3 q4 q5 q6 q7 q8 q9])
    (addo [q1 q2 q3] 15)
    (addo [q4 q5 q6] 15)
    (addo [q7 q8 q9] 15)
    (addo [q1 q4 q7] 15)
    (addo [q2 q5 q8] 15)
    (addo [q3 q6 q9] 15)
    (addo [q1 q5 q9] 15)
    (addo [q3 q5 q7] 15)))

(defne largesto
  "Succeeds if the largest element in lst unifies
  to result."
  [lst result]
  ([[x] x])
  ([[head . tail] head]
   (fresh [temp]
     (largesto tail temp)
     (fd/> head temp)))
  ([[head . tail] temp]
   (largesto tail temp)
   (fd/>= temp head)))
