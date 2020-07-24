(define size 2)

(define pi 3.14159)
(define radius 10)
(define circumference (* 2 pi radius))

(define (square x) (* x x))

(define (always-three x) 3)

(define (sum-of-squares x y)
    (+ (square x) (square y)))

(define (abs x)
    (cond ((< x 0) (- x))
          (else x)))

(define (abs-if x)
    (if (< x 0) (- x) x))

(/
    (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
    (* 3 (- 6 2) (- 2 7)))

(define (sum-larger-squares x y z)
    (cond ((> x y) (sum-of-squares x (max y z)))
          (else    (sum-of-squares y (max x z)))))

(define (average x y)
    (/ (+ x y) 2))

(define (sqrt x)
    (define (iter guess)
        (if (good-enough? guess)
            guess
            (iter (improve guess))))

    (define (good-enough? guess)
        (< (abs (- (improve guess) guess)) 0.001))

    (define (improve guess)
        (average guess (/ x guess)))
    
    (iter 1.0))

(define (cbrt x)
    (define (iter guess)
        (if (good-enough? guess)
            guess
            (iter (improve guess))))

    (define (good-enough? guess)
        (< (abs (- (improve guess) guess)) 0.001))

    (define (improve guess)
        (/ (+ (/ x (square guess)) 
          (* 2 guess))
       3))

    (iter 1.0))