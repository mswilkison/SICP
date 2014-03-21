(define (last-pair x)
  (if (null? (cdr x))
    x
    (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c))) ; '(a b c a b c ... )
(last-pair z) ; This will cause an infinite loop since the end of x points back
	      ; to the beginning of x
