; (magnitude z)
; (apply-generic 'magnitude '(complex rectangular 3 4))
; ((get 'magnitude '(complex)) '(rectangular 3 4))
; (magnitude '(rectangular 3 4))
; (apply-generic 'magnitude '(rectangular 3 4))
; ((get 'magnitude '(rectangular)) (3 4))
; (magnitude (3 4))
; 3
