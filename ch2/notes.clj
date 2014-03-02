;; Chapter 2

(defn linear-combination [a b x y]
  (+ (* a x) (* b y)))

;(defn linear-combination [a b x y]
;  (add (mult a x) (mult b y)))

;; 2.1 Introduction to Data Abstraction

; 2.1.1 Example: Arithmetic Operations for Rational Numbers
(defn make-rat [n d] (cons n [d]))

(defn numer [x] (first x))

(defn denom [x] (last x))

(defn add-rat [x y]
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn sub-rat [x y]
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn mul-rat [x y]
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(defn div-rat [x y]
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(defn equal-rat? [x y]
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(def x [1 2])
(first x)
(last x)

(def x [1 2])
(def y [3 4])
(def z (cons x [y]))
z
(first (first z))
(first (last z))

(defn print-rat [x]
  (newline)
  (print (numer x))
  (print "/")
  (print (denom x)))

(def one-half (make-rat 1 2))
(print-rat one-half)

(def one-third (make-rat 1 3))
(print-rat (add-rat one-half one-third))
(print-rat (mul-rat one-half one-third))
(print-rat (add-rat one-third one-third))

(defn gcd [a b]
  (if (= b 0)
    a
    (gcd b (mod a b))))

(defn make-rat [n d]
  (let [g (gcd n d)]
    (cons (/ n g) [(/ d g)])))

(print-rat (add-rat one-third one-third))

(defn make-rat [n d]
  (cons n [d]))

(defn numer [x]
  (let [g (gcd (first x) (last x))]
    (/ (first x) g)))

(defn denom [x]
  (let [g (gcd (first x) (last x))]
    (/ (last x) g)))

; 2.1.3 What Is Meant by Data?

;(defn cons [x y]
; (defn dispatch [m]
;    (cond (= m 0) x
;          (= m 1) y
;          :else (throw (Exception. (str "Argument not 0 or 1 -- CONS" m)))))
;  dispatch)

(defn car [z] (z 0))

(defn cdr [z] (z 1))

; 2.1.4 Extended Exercise: Interval Arithmetic
(defn make-interval [a b] (cons a [b]))

(defn upper-bound [interval]
  (last interval))

(defn lower-bound [interval]
  (first interval))

(defn add-interval [x y]
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(defn mul-interval [x y]
  (let [p1 (* (lower-bound x) (lower-bound y))
        p2 (* (lower-bound x) (upper-bound y))
        p3 (* (upper-bound x) (lower-bound y))
        p4 (* (upper-bound x) (upper-bound y))]
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(defn div-interval [x y]
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(defn make-center-width [c w]
  (make-interval (- c w) (+ c w)))

(defn center [i]
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(defn width [i]
  (/ (- (upper-bound i) (lower-bound i)) 2))

(defn par1 [r1 r2]
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(defn par2 [r1 r2]
  (let [one (make-interval 1 1)]
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

;; 2.2 Hierarchical Data and the Closure Property

; 2.2.1 Representing Sequences
(cons 1
      (cons 2
            (cons 3
                  (cons 4 [nil]))))

(list 1 2 3 4)

(def one-through-four (list 1 2 3 4))
one-through-four

(first one-through-four)
(rest one-through-four)
(first (rest one-through-four))
(cons 10 one-through-four)
(cons 5 one-through-four)

(defn list-ref [items n]
  (if (= n 0)
    (first items)
    (list-ref (rest items) (dec n))))

(def squares '(1 4 9 16 25))
(list-ref squares 3)

(defn length [items]
  (if (empty? items)
    0
    (+ 1 (length (rest items)))))

(def odds '(1 3 5 7))
(length odds)

(defn length [items]
  (defn length-iter [a counter]
    (if (empty? a)
      counter
      (length-iter (rest a) (inc counter))))
  (length-iter items 0))

(defn append [list1 list2]
  (if (empty? list1)
    list2
    (cons (first list1) (append (rest list1) list2))))

(append squares odds)
(append odds squares)

(defn scale-list [items factor]
  (if (empty? items)
    ()
    (cons (* (first items) factor)
          (scale-list (rest items) factor))))

(scale-list '(1 2 3 4 5) 10)

(map + '(1 2 3) '(40 50 60) '(700 800 900))

(map (fn [x y] (+ x (* 2 y)))
     '(1 2 3)
     '(4 5 6))

;(defn map [proc items]
;  (if (empty? items)
;    ()
;    (cons (proc (first items))
;          (map proc (rest items)))))

(defn abs [x]
  (if (< x 0)
    (- x)
    x))

(map abs (list -10 2.5 -11.6 17))

(map (fn [x] (* x x))
     '(1 2 3 4))

(defn scale-list [items factor]
  (map (fn [x] (* x factor))
       items))

; 2.2.2 Hierarchical Structures
(cons '(1 2) '(3 4))

(def x (cons (list 1 2) (list 3 4)))
(length x)

(defn count-leaves [x]
  (cond (not (seq? x)) 1
        (empty? x) 0
        :else (+ (count-leaves (first x))
                 (count-leaves (rest x)))))

(count-leaves x)

(list x x)
(length (list x x))
(count-leaves (list x x))

(defn scale-tree [tree factor]
  (cond (not (seq? tree)) (* tree factor)
        (empty? tree) ()
        :else (cons (scale-tree (first tree) factor)
                    (scale-tree (rest tree) factor))))

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)

(defn scale-tree [tree factor]
  (map (fn [sub-tree]
         (if (seq? sub-tree)
           (scale-tree sub-tree factor)
           (* sub-tree factor)))
       tree))

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)

; 2.2.3 Sequences as Conventional Interfaces
(defn square [x]
  (* x x))

(defn sum-odd-squares [tree]
  (cond (not (seq? tree))
          (if (odd? tree) (square tree) 0)
        (empty? tree) 0
        :else (+ (sum-odd-squares (first tree))
                 (sum-odd-squares (rest tree)))))

(defn fib [n]
  (cond (= n 0) 0
        (= n 1) 1
        :else (+ (fib (- n 1))
                 (fib (- n 2)))))

(defn even-fibs [n]
  (defn step [k]
    (if (> k n)
      ()
      (let [f (fib k)]
        (if (even? f)
          (cons f (step (inc k)))
          (step (inc k))))))
  (step 0))

(map square (list 1 2 3 4 5))

(defn my-filter [predicate lst]
  (cond (empty? lst) ()
        (predicate (first lst))
          (cons (first lst)
                (filter predicate (rest lst)))
       :else (my-filter predicate (rest lst))))

(my-filter odd? (list 1 2 3 4 5))

(defn accumulate [op initial lst]
  (if (empty? lst)
    initial
    (op (first lst)
        (accumulate op initial (rest lst)))))

(accumulate + 0 (list 1 2 3 4 5))
(accumulate * 1 (list 1 2 3 4 5))
(accumulate cons () (list 1 2 3 4 5))

(defn enumerate-interval [low high]
  (if (> low high)
    ()
    (cons low (enumerate-interval (inc low) high))))

(defn enumerate-tree [tree]
  (cond (not (seq? tree)) (list tree)
        (empty? tree) ()
        :else (append (enumerate-tree (first tree))
                      (enumerate-tree (rest tree)))))

(defn sum-odd-squares [tree]
  (accumulate +
              0
              (map square
                   (filter odd?
                      (enumerate-tree tree)))))

(defn even-fibs [n]
  (accumulate cons
              nil
              (filter even?
                      (map fib
                           (enumerate-interval 0 n)))))

(defn list-fib-squares [n]
  (accumulate cons
              nil
              (map square
                   (map fib
                        (enumerate-interval 0 n)))))

(list-fib-squares 10)

(defn product-of-squares-of-odd-elements [lst]
  (accumulate *
              1
              (map square
                   (filter odd? lst))))

(product-of-squares-of-odd-elements (list 1 2 3 4 5))

;(defn salary-of-heighest-paid-programmer [records]
;  (accumulate max
;              0
;              (map salary
;                   (filter programmer? records))))


; Nested Mappings

;(accumulate append
;            ()
;            (map (fn [i]
;                   (map (fn [j] (list i j))
;                        (enumerate-interval 1 (dec i))))
;                 (enumerate-interval 1 n)))

(defn flatmap [proc lst]
  (accumulate append () (map proc lst)))

(defn divides? [a b]
  (= (mod b a) 0))

(defn smallest-divisor [n]
  (defn find-divisor [test-divisor]
    (cond (> (square test-divisor) n) n
          (divides? test-divisor n) test-divisor
          :else (find-divisor (+ test-divisor 1))))
  (find-divisor 2))

(defn prime? [n]
  (= n (smallest-divisor n)))

(defn prime-sum? [pair]
  (prime? (+ (first pair) (last pair))))

(defn make-pair-sum [pair]
  (list (first pair) (last pair) (+ (first pair) (last pair))))

(defn prime-sum-pairs [n]
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (fn [i]
                  (map (fn [j] (list i j))
                       (enumerate-interval 1 (dec i))))
                (enumerate-interval 1 n)))))

(prime-sum-pairs 6)

(defn remove-item [item lst]
  (filter (fn [x] (not (= x item)))
          lst))

(defn permutations [s]
  (if (empty? s)
    (list ())
    (flatmap (fn [x]
               (map (fn [p] (cons x p))
                    (permutations (remove-item x s))))
             s)))

(permutations (list 1 2 3))

; 2.2.4 Example: A Picture Language
(comment
  (def wave2 (beside wave (flip-vert wave)))
  (def wave4 (below (wave2 wave2)))

  (defn flipped-pairs [painter]
    (let [painter2 (beside painter (flip-vert painter))]
      (below painter2 painter2)))

  (def wave4 (flipped-pairs wave))

  (defn right-split [painter n]
    (if (= n 0)
      painter
      (let [smaller (right-split painter (dec n))]
        (beside painter (below smaller smaller)))))

  (defn corner-split [painter n]
    (if (= n 0)
      painter
      (let [up (up-split painter (dec n))
            right (right-split painter (dec n))]
        (let [top-left (beside up up)
              bottom-right (below right right)
              corner (corner-split painter (dec n))]
          (beside (below painter top-left)
                  (below bottom-right corner))))))

  (defn square-limit [painter n]
    (let [quarter (corner-split painter n)]
      (let [half (beside (flip-horiz quarter) quarter)]
        (below (flip-vert half) half))))

  (defn square-of-four [tl tr bl br]
    (fn [painter]
      (let [top (beside (tl painter) (tr painter))
            bottom (beside (bl painter) (br painter))]
        (below bottom top))))

  (defn flipped-pairs [painter]
    (let [combine4 (square-of-four identity flip-vert
                                   identity flip-vert)]
      (combine4 painter)))

  (def flipped-pairs
    (square-of-four identity flip-vert identity flip-vert))

  (defn square-limit [painter n]
    (let [combine4 (square-of-four flip-horiz identity
                                   rotate 180 flip-vert)]
      (combine4 (corner-split painter n))))

  (def square-limit
    (square-of-four flip-horiz identity
                    rotate180 flip-vert))
)

; Frames
(defn make-vect [x-cor y-cor]
  (cons x-cor [y-cor]))

(defn xcor-vect [v]
  (first v))

(defn ycor-vect [v]
  (first (rest v)))

(defn bi-op-vect [op v1 v2]
  (make-vect (op (xcor-vect v1)
                 (xcor-vect v2))
             (op (ycor-vect v1)
                 (ycor-vect v2))))

(defn add-vect [v1 v2]
  (bi-op-vect + v1 v2))

(defn sub-vect [v1 v2]
  (bi-op-vect - v1 v2))

(defn scale-vect [s v]
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

(defn make-frame [origin edge1 edge2]
  (list origin edge1 edge2))

(defn make-frame [origin edge1 edge2]
  (cons origin (cons edge1 [edge2])))

(defn origin-frame [frame]
  (first frame))

(defn edge1-frame [frame]
  (first (rest frame)))

(defn edge2-frame [frame]
  (first (rest (rest frame))))

(defn frame-coord-map [frame]
  (fn [v]
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

(comment
  (defn segments->painter [segment-list]
    (fn [frame]
      (doall
       (fn [segment]
         (draw-line
          ((frame-coord-map frame) (start-segment segment))
          ((frame-coord-map frame) (end-segment segment))))
       segment-list))))

; Transforming and combining painters
(defn transform-painter [painter origin corner1 corner2]
  (fn [frame]
    (let [m (frame-coord-map frame)]
      (let [new-origin (m origin)]
        (painter
          (make-frame new-origin
                      (sub-vect (m corner1) new-origin)
                      (sub-vect (m corner2) new-origin)))))))

(defn flip-vert [painter]
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(defn shrink-to-upper-right [painter]
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))

(defn rotate90 [painter]
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(defn squash-inwards [painter]
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

(defn beside [painter1 painter2]
  (let [split-point (make-vect 0.5 0.0)]
    (let [paint-left (transform-painter painter1
                                        (make-vect 0.0 0.0)
                                        split-point
                                        (make-vect 0.0 1.0))
          paint-right (transform-painter painter2
                                         split-point
                                         (make-vect 1.0 0.0)
                                         (make-vect 0.5 1.0))]
      (fn [frame]
        (paint-left frame)
        (paint-right frame)))))

;; 2.3 Symbolic Data
; 2.3.1 Quotation
(def a 1)
(def b 2)

(list a b)
(list 'a 'b)
(list 'a b)

(first '(a b c))
(rest '(a b c))

(quote a)
(quote (a b c))
(list 'car (list 'quote '(a b c)))

'()

(defn memq [item x]
  (cond (empty? x) false
        (= item (first x)) x
        :else (memq item (rest x))))

(memq 'apple '(pear banana prune))
(memq 'apple '(x (apple sauce) y apple pear))

; 2.3.2 Example: Symbolic Differentation
(defn variable? [x]
  (symbol? x))

(defn same-variable? [v1 v2]
  (and (variable? v1) (variable? v2)) (= v1 v2))

(defn make-sum [a1 a2]
  (list '+ a1 a2))

(defn make-product [m1 m2]
  (list '* m1 m2))

(defn sum? [x]
  (and (seq? x) (= (first x) '+)))

(defn addend [s] (second s))

(defn augend [s] (last s))

(defn product? [x]
  (and (seq? x) (= (first x) '*)))

(defn multiplier [p]
  (second p))

(defn multiplicand [p]
  (last p))

(defn deriv [exp var]
  (cond (number? exp) 0
        (variable? exp) (if (same-variable? exp var) 1 0)
        (sum? exp) (make-sum (deriv (addend exp) var)
                             (deriv (augend exp) var))
        (product? exp) (make-sum (make-product (multiplier exp)
                                               (deriv (multiplicand exp) var))
                                 (make-product (deriv (multiplier exp) var)
                                               (multiplicand exp)))
        :else (throw (Exception. "unknown expression type -- DERIV" exp))))

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)

(defn =number? [exp number]
  (and (number? exp) (= exp number)))

(defn make-sum [a1 a2]
  (cond (=number? a1 0) a2
        (=number? a2 0) a1
        (and (number? a1) (number? a2)) (+ a1 a2)
        :else (list '+ a1 a2)))

(defn make-product [m1 m2]
  (cond (or (=number? m1 0) (=number? m2 0)) 0
        (=number? m1 1) m2
        (=number? m2 1) m1
        (and (number? m1) (number? m2)) (* m1 m2)
        :else (list '* m1 m2)))

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)

; 2.3.3 Example: Representing Sets
(defn element-of-set? [x my-set]
  (cond (empty? my-set) false
        (= x (first my-set)) true
        :else (element-of-set? x (rest my-set))))

(defn adjoin-set [x my-set]
  (if (element-of-set? x my-set)
    my-set
    (cons x my-set)))

(defn intersection-set [set1 set2]
  (cond (or (empty? set1) (empty? set2)) '()
        (element-of-set? (first set1) set2)
          (cons (first set1)
                (intersection-set (rest set1) set2))
        :else (intersection-set (rest set1) set2)))
