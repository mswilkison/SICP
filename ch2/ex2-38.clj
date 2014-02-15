(defn fold-right [op initial lst]
  (if (empty? lst)
    initial
    (op (first lst)
        (fold-right op initial (rest lst)))))

(defn fold-left [op initial lst]
  (defn iter [result remaining]
    (if (empty? remaining)
      result
      (iter (op result (first remaining))
            (rest remaining))))
  (iter initial lst))

(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))

(fold-right list () (list 1 2 3))
(fold-left list () (list 1 2 3))

; If op = + or *, fold-left and fold-right will produce
; the same value
(fold-right + 0 '(1 2 3))
(fold-left + 0 '(1 2 3))

(fold-right * 1 '(1 2 3))
(fold-left * 1 '(1 2 3))
