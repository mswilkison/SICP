(define (make-deque)
  (cons '() '()))

(define (front-ptr deque)
  (car deque))

(define (rear-ptr deque)
  (cdr deque))

(define (empty-deque? deque)
  (null? (front-ptr deque)))

(define (front-deque deque)
  (if (empty-deque? deque)
    (error "FRONT called with an empty deque" deque)
    (car (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
    (error "REAR called with an empty deque" deque)))

(define (set-front-ptr! deque item)
  (set-car! deque item))

(define (set-rear-ptr! deque item)
  (set-cdr! deque item))

(define (front-insert-deque! deque item)
  (let ((new-pair (cons item '())))
    (cond ((empty-deque? deque)
	   (set-front-ptr! deque new-pair)
	   (set-rear-ptr! deque new-pair)
	   deque)
	  (else
	    (append! new-pair (front-ptr deque))
	    (set-front-ptr! deque new-pair)
	    deque))))

(define (rear-insert-deque! deque item)
  (let ((new-pair (cons item '())))
    (cond ((empty-deque? deque)
	   (set-front-ptr! deque new-pair)
	   (set-rear-ptr! deque new-pair)
	   deque)
	  (else
	    (set-cdr! (rear-ptr deque) new-pair)
	    (set-rear-ptr! deque new-pair)
	    deque))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
	 (error "DELETE! called with an empty deque" deque))
	(else
	  (set-front-ptr! deque (cdr (front-ptr deque)))
	  deque)))

; finish rear-delete-deque!
(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
	 (error "DELETE! called with an empty deque" deque))
	(else
	  (set-rear-ptr! deque (...))
	  deque)))
