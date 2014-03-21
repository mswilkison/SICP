(define (count-pairs x)
  (if (not (pair? x))
    0
    (+ (count-pairs (car x))
       (count-pairs (cdr x))
       1)))

(count-pairs '(x y z)) ; 3
(count-pairs '((x) (x))) ; 4
(count-pairs '(((x) x) (x) x)) ; 7
(define cycle '(x x))
(set-cdr! cycle cycle)
(count-pairs cycle) ; infinite loop
