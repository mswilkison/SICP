(define f
  (let ((start -1))
    (define (dispatch num)
      (if (= start -1)
	(begin (set! start num)
	       start)
	start))
    dispatch))
