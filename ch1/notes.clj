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

;; 1.2.2 Tree Recursion
(defn fib [n]
  (cond (= n 0) 0
        (= n 1) 1
        :else (+ (fib (- n 1)
                      (- n 2)))))

(defn fib-iter [a b count]
  (if (= count 0)
    b
    (fib-iter (+ a b) a (- count 1))))

(defn fib [n]
  (defn fib-iter [a b count]
    (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

(fib 4)

; Example: Counting Change
(defn count-change [amount]
  (defn first-denomination [kinds-of-coins]
    (cond (= kinds-of-coins 1) 1
          (= kinds-of-coins 2) 5
          (= kinds-of-coins 3) 10
          (= kinds-of-coins 4) 25
          (= kinds-of-coins 5) 50))
  (defn cc [amount kinds-of-coins]
    (cond (= amount 0) 1
          (or (< amount 0) (= kinds-of-coins 0)) 0
          :else (+ (cc amount
                       (- kinds-of-coins 1))
                   (cc (- amount
                          (first-denomination kinds-of-coins))
                       kinds-of-coins))))
  (cc amount 5))

(count-change 100)

; 1.2.3 Orders of Growth

; 1.2.4 Exponentiation
(defn expt [b n]
  (if (= n 0)
    1
    (* b (expt b (- n 1)))))

(defn expt [b n]
  (defn expt-iter [counter product]
    (if (= counter 0)
      product
      (expt-iter (- counter 1) (* b product))))
  (expt-iter n 1))

(defn fast-expt [b n]
  (cond (= n 0) 1
        (even? n) (square (fast-expt b (/ n 2)))
        :else (* b (fast-expt b (- n 1)))))

; 1.2.5 Greatest Common Divisors
(defn gcd [a b]
  (if (= b 0)
    a
    (gcd b (mod a b))))

; 1.2.6 Example: Testing for Primality
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

(defn expmod [base exp m]
  (cond (= exp 0) 1
        (even? exp)
          (mod (square (expmod base (/ exp 2) m)) m)
        :else (mod (* base (expmod base (- exp 1) m)) m)))

(defn fermat-test [n]
  (defn try-it [a]
    (= (expmod a n n) a))
  (try-it (+ 1 (rand-int n))))

(defn fast-prime? [n times]
  (cond (= times 0) true
        (fermat-test n) (fast-prime? n (- times 1))
        :else false))

;; 1.3 Formulating Abstractions with Higher-Order Procedures

; 1.3.1 Procedures as Arguments
(defn cube [x]
  (* x x x))
(defn sum-integers [a b]
  (if (> a b)
    0
    (+ a (sum-integers (+ a 1) b))))

(defn sum-cubes [a b]
  (if (> a b)
    0
    (+ (cube a) (sum-cubes (+ a 1) b))))

(defn pi-sum [a b]
  (if (> a b)
    0
    (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

(defn sum [term a step b]
  (if (> a b)
    0
    (+ (term a)
       (sum term (step a) step b))))

(defn inc [n]
  (+ n 1))

(defn sum-cubes [a b]
  (sum cube a inc b))

(sum-cubes 1 10)

(defn identity [x] x)

(defn sum-integers [a b]
  (sum identity a inc b))

(sum-integers 1 10)

(defn pi-sum [a b]
  (defn pi-term [x]
    (/ 1.0 (* x (+ x 2))))
  (defn pi-next [x]
    (+ x 4))
  (sum pi-term a pi-next b))

(* 8 (pi-sum 1 1000)) ; approximation of pi

(defn integral [f a b dx]
  (defn add-dx [x] (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(integral cube 0 1 0.01)
(integral cube 0 1 0.001)

; 1.3.2 Constructing Procedures Using Lambda
(fn [x] (+ x 4))
(fn [x] (/ 1.0 (* x (+ x 2))))

(defn pi-sum [a b]
  (sum (fn [x] (/ 1.0 (* x (+ x 2))))
       a
       (fn [x] (+ x 4))
       b))

(defn integral [f a b dx]
  (* (sum f
          (+ a (/ dx 2.0))
          (fn [x] (+ x dx))
          b)
     dx))

(defn f [x y]
  (defn f-helper [a b]
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (f-helper (+ 1 (* x y))
            (- 1 y)))

(defn f [x y]
  (fn [a b]
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (+ 1 (* x y))
  (- 1 y))

(defn f [x y]
  (let [a (+ 1 (* x y))
        b (- 1 y)]
    (+ (* x (square a))
       (* y b)
       (* a b))))

(defn f [x y]
  (def a (+ 1 (* x y)))
  (def b (- 1 y))
  (+ (* x (square a))
     (* y b)
     (* a b)))

; 1.3.3 Procedures as General Methods

; - Finding roots of equations by the half-interval method
(defn close-enough? [x y]
  (< (abs (- x y)) 0.001))

(defn positive? [x]
  (> x 0))

(defn negative? [x]
  (not (positive? x)))

(defn search [f neg-point pos-point]
  (let [midpoint (average neg-point pos-point)]
    (if (close-enough? neg-point pos-point)
      midpoint
      (let [test-value (f midpoint)]
        (cond (positive? test-value) (search f neg-point midpoint)
              (negative? test-value) (search f midpoint pos-point)
              :else midpoint)))))

(defn half-interval-method [f a b]
  (let [a-value (f a)
        b-value (f b)]
    (cond (and (negative? a-value) (positive? b-value)) (search f a b)
          (and (negative? b-value) (positive? a-value)) (search f b a)
          :else (throw (Exception. (str "Values are not of opposite sign" a b))))))

(half-interval-method #(Math/sin %) 2.0 4.0)
(half-interval-method (fn [x] (- (* x x x) (* 2 x) 3))
                      1.0
                      2.0)

; - Finding fixed points of functions
(def tolerance 0.00001)

(defn fixed-point [f first-guess]
  (defn close-enough? [v1 v2]
    (< (abs (- v1 v2)) tolerance))
  (defn try-it [guess]
    (let [step (f guess)]
      (if (close-enough? guess step)
        step
        (try-it step))))
  (try-it first-guess))

(fixed-point #(Math/cos %) 1.0)
(fixed-point (fn [y] (+ (Math/sin y) (Math/cos y))) 1.0)

(defn sqrt [x]
  (fixed-point (fn [y] (average y (/ x y)))
               1.0))

; 1.3.4 Proceures as Returned Values
(defn average-damp [f]
  (fn [x] (average x (f x))))

((average-damp square) 10)

(defn sqrt [x]
  (fixed-point (average-damp (fn [y] (/ x y))) 1.0))

(defn cube-root [x]
  (fixed-point (average-damp (fn [y] (/ x (square y)))) 1.0))

(def dx 0.00001)

(defn deriv [g]
  (fn [x] (/ (- (g (+ x dx)) (g x))
             dx)))

((deriv cube) 5)

(defn newton-transform [g]
  (fn [x]
    (- x (/ (g x) ((deriv g) x)))))

(defn newtons-method [g guess]
  (fixed-point (newton-transform g) guess))

(defn sqrt [x]
  (newtons-method (fn [y] (- (square y) x)) 1.0))

(defn fixed-point-of-transform [g transform guess]
  (fixed-point (transform g) guess))

(defn sqrt [x]
  (fixed-point-of-transform (fn [y] (/ x y))
                            average-damp
                            1.0))

(defn sqrt[x]
  (fixed-point-of-transform (fn [y] (- (square y) x))
                            newton-transform
                            1.0))
