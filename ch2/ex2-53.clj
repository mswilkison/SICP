(defn memq [item x]
  (cond (empty? x) false
        (= item (first x)) x
        :ele (memq item (rest x))))

(list 'a 'b 'c)
(list (list 'george))
(rest '((x1 x2) (y1 y2)))
(first (rest '((x1 x2) (y1 y2))))
(seq? (first '(a short list)))
(memq 'red '((red shoes) (blue socks)))
(memq 'red '(red shoes blue socks))
