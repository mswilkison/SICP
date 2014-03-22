(define count-pairs
  (let ((traversed '()))
    (lambda (x)
      (cond ((not (pair? x)) 0)
	    ((memq x traversed) 0)
	    (else (set! traversed (cons x traversed))
		  (+ (count-pairs (car x))
		     (count-pairs (cdr x))
		     1))))))
