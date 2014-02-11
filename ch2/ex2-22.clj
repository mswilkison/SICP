(defn square [x]
  (* x x))

(defn square-list [items]
  (defn iter [things answer]
    (if (empty? things)
      answer
      (iter (rest things)
            (cons (square (first things))
                  answer))))
  (iter items ()))

(square-list '(1 2 3 4))
; This creates an answer list in reverse order
; because as we traverse the input list, we cons
; the squares on to the front of the answer list

(defn square-list-2 [items]
  (defn iter [things answer]
    (if (empty? things)
      answer
      (iter (rest things)
            (cons answer
                  [(square (first things))]))))
  (iter items ()))

(square-list-2 '(1 2 3 4))
; This strange result happens because, at each
; iteration, we cons a squared integer to the
; end of a list, resulting in a set of nested
; lists
