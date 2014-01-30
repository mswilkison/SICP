; Prove that Fib(n) is the closest integer to Phi^n/sqrt(5),
; where Phi = (1 + sqrt(5))/2

(defn fib [n]
  (cond (= n 0) 0
        (= n 1) 1
        :else (+ (fib (- n 1))
                 (fib (- n 2)))))

(defn exp [base power]
  (cond (= power 0) 1
        (= power 1) base
        :else (* base
                 (exp base (- power 1)))))

(def phi
  (/ (+ 1 (Math/sqrt 5)) 2))

(def psi
  (/ (- 1 (Math/sqrt 5)) 2))

(defn phi-psi [n]
  (/ (- (exp phi n) (exp psi n)) (Math/sqrt 5)))

(fib 0)
(phi-psi 0)

(fib 1)
(phi-psi 1)

(fib 2)
(phi-psi 2)
