;; Chapter 1

;; 1.1 The Elements of Programming

;; 1.1.1 Expressions
(+ 137 439)

(- 1000 334)

(* 5 99)

(/ 10 5)

(+ 2.7 10)

(+ 21 35 12 7)

(* 25 4 12)

(+ (* 3 5) (- 10 6))

(+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6))

(+ (* 3
      (+ (* 2 4)
         (+ 3 5)))
   (+ (- 10 7)
      6))

;; 1.1.2 Naming and the Environment
(def size 2)

(* 5 size)

(def pi 3.14159)

(def radius 10)

(* pi (* radius radius))

(def circumference (* 2 pi radius))

circumference

;; 1.1.3 Evaluating Combinations
(* (+ 2 (* 4 6))
   (+ 3 5 7))

;; 1.1.4 Compound Procedures
(defn square [x] (* x x))

(square 21)
(square (+ 2 5))
(square (square 3))

(defn sum-of-squares [x y]
  (+ (square x) (square y)))

(sum-of-squares 3 4)

(defn f [a]
  (sum-of-squares (+ a 1) (* a 2)))

(f 5)

;; 1.1.5 The Substitution Model for Procedure Application

;; 1.1.6 Conditional Expressions and Predicates
(defn abs [x]
  (cond (> x 0) x
        (= x 0) 0
        (< x 0) (- x)))

(defn abs [x]
  (cond (< x 0) (- x)
        :else x))

(defn abs [x]
  (if (< x 0) (- x)
    x))

(defn >= [x y]
  (or (> x y) (= x y)))

(defn >= [x y]
  (not (< x y)))

;; 1.1.7 Example: Square Roots by Newton's Method
(defn average [x y]
  (/ (+ x y) 2))

(defn improve [guess x]
  (average guess (/ x guess)))

(defn good-enough? [guess x]
  (< (abs (- (square guess) x)) 0.001))

(defn sqrt-iter [guess x]
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(defn sqrt [x]
  (sqrt-iter 1.0 x))

(sqrt 9)
(sqrt (+ 100 37))
(sqrt (+ (sqrt 2) (sqrt 3)))
(square (sqrt 1000))

;; 1.1.8 Procedures as Black-Box Abstractions
(defn sqrt [x]
  (defn good-enough? [guess x]
    (< (abs (- (square guess) x)) 0.001))
  (defn improve [guess x]
    (average guess (/ x guess)))
  (defn sqrt-iter [guess x]
    (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))

(defn sqrt [x]
  (defn good-enough? [guess]
    (< (abs (- (square guess) x)) 0.001))
  (defn improve [guess]
    (average guess (/ x guess)))
  (defn sqrt-iter [guess]
    (if (good-enough? guess)
      guess
      (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(sqrt 9)

;; 1.2 Procedures and the Processes They Generate

;; 1.2.1 Linear Recursion and Iteration
(defn factorial [n]
  (if (= n 1)
    1
    (* n (factorial (- n 1)))))

(defn factorial [n]
  (defn iter [product counter]
    (if (> counter n)
      product
      (iter (* counter product)
            (+ counter 1))))
  (iter 1 1))
