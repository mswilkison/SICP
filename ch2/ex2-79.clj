; generic operation
(defn equ? [tag x y]
  (apply-generic 'equ? x y))

; scheme-number
(put 'equ? '(scheme-number scheme-number) =)

; rational
(defn equ-rat [r1 r2]
  (and (= (numer r1) (numer r2))
       (= (denom r1) (denom r2))))
(put 'equ? '(rational rational) equ-rat)

; complex
(defn equ-complex [c1 c2]
  (and (= (real-part c1) (real-part c2))
       (= (imag-part c1) (imag-part c2))))
(put 'equ? '(complex complex) equ-complex)
