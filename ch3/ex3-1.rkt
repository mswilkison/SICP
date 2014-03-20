(define (make-accumulator sum)
  (define (add arg)
    (set! sum (+ sum arg))
    sum)
  add)

(A 10)
(A 10)
