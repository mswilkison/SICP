; (a) This is a data-directed implementation of deriv in which the operator is treated as the tag,
; and the correct procedure is extracted from the 2D table using get.
; We can't assimilate the predicates number? and same-variable? into
; the data-directed dispatch because they do not fit the expected format of '(<operator> <operands>)

; (b)
(defn install-deriv-package
  (defn =number? [exp number]
    (and (number? exp) (= exp number)))

  ;; sum procedures
  (defn make-sum [a1 a2]
    (cond (=number? a1 0) a2
          (=number? a2 0) a1
          (and (number? a1) (number? a2)) (+ a1 a2)
          :else (list '+ a1 a2)))
  (defn addend [operands] (first operand))
  (defn augend [operands] (second operands))
  (defn deriv-sum [operands var]
    (make-sum (deriv (addend operands) var)
              (deriv (augend operands) var)))

  ;; product procedures
  (defn make-product [m1 m2]
    (cond (or (=number? m1 0) (=number? m2 0)) 0
          (=number? m1 1) m2
          (=number? m2 1) m1
          (and (number? m1) (number? m2)) (* m1 m2)
          :else (list '* m1 m2)))
  (defn multiplier [operands] (first operands))
  (defn multiplicand [operands] (second operands))
  (defn deriv-product [operands var]
    (make-sum (make-product (multiplier operands)
                            (deriv (multiplicand operands) var))
              (make-product (deriv (multiplier operands) var)
                            (multiplicand operands))))

  ;; exponentiation procedures
  (defn make-exponentiation [e1 e2]
    (cond (=number? e2 0) 1
          (=number? e2 1) e1
          :else (list '** e1 e2)))
  (defn base [operands] (first operands))
  (defn exponent [operands] (second operands))
  (defn deriv-exponentiation [operands var]
    (make-product (make-product (exponent operands)
                                (make-exponentiation (base operands)
                                                     (make-sum (exponent operands) -1)))
                  (deriv (base operands) var)))

  ;; interface
  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-product)
  (put 'deriv '** deriv-exponentiation)
  'done)

(defn operator [exp] (first exp))

(defn operands [exp] (rest exp))

(defn deriv [exp var]
  (cond (number? exp) 0
        (variable? exp) (if (same-variable? exp var) 1 0)
        :else ((get 'deriv (operator exp)) (operands exp)
                                           var)))

; (d) If we indexd the procedure in th opposite way as specified,
; the order of arguments given to `put` would need to be switched.
