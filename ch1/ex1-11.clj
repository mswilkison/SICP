;; f(n)
;;      = n, if n < 3
;;      = f(n - 1) + 2f(n - 2) + 3f(n - 3), if n >= 3n

(defn recursive-f [n]
  (if (< n 3)
    n
    (+ (recursive-f (- n 1))
       (* 2 (recursive-f (- n 2)))
       (* 3 (recursive-f (- n 3))))))

(defn iterative-f [n]
  (defn iter [a b c count]
    (if (< count 3)
      a
      (iter (+ a (* 2 b) (* 3 c))
            a
            b
            (- count 1))))
  (if (< n 3)
    n
    (iter 2 1 0 n)))
