(define (make-monitored f)
  (define count 0)
  (define (mf m)
    (cond ((eq? m 'how-many-calls) count)
	  ((eq? m 'reset-count) (begin
				  (set! count 0)
				  count))
	  (else (begin
		  (set! count (+ count 1))
		  (f m)))))
  mf)
