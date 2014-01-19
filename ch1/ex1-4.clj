;; This function accepts two args `a` and `b`.
;; If `b` is positive it adds `b` to `a`.
;; Otherwise it subtracts `b` from `a`.

(defn a-plus-abs-b [a b]
  ((if (> b 0) + -) a b))