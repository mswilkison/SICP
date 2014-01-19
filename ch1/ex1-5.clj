(def p p)

(defn test [x y]
  (if (= x 0)
    0
    y))

(test 0 p)

;; An interpreter using applicative-order evaluation
;; will return 0 as the if statement will return 0
;; without attempting to evaluate the else case.
;; An interpreter using normal-order evaluation will
;; get stuck in an infinite loop when it tries to
;; evaluate `p`.