(define rand
  (let ((x 0))
    (define (dispatch m)
      (cond ((eq? m 'generate)
	     (begin (set! x (+ x 1))
		    x))
	    ((eq? m 'reset)
	     (lambda (new-x) (set! x new-x)))))
    dispatch))
