(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-list)
      (define (lookup-1 keys table)
	(let ((subtable (assoc (car keys) (cdr table))))
	  (if subtable
	    (if (null? (cdr keys))
	      (cdr subtable)
	      (lookup1 (cdr keys) subtable))
	    false)))
      (lookup1 key-list local-table))

    (define (insert! key-list value)
      (define (make-entry keys)
	(if (null? (cdr keys))
	  (cons (car keys) value)
	  (list (car keys) (make-entry (cdr keys)))))
      (define (insert1 keys table)
	(let ((subtable (assoc (car keys) (cdr table))))
	  (if subtable
	    (if (null? (cdr keys))
	      (set-cdr! subtable value)
	      (insert1 (cdr keys) subtable))
	    (set-cdr! table
		      (cons (make-entry keys)
			    (cdr table))))))
      (insert1 key-list local-table)
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc!) insert!)
	    (else (error "unknown operation -- TABLE" m))))
    dispatch))