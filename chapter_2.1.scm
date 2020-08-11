(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))

(define (signum x)
    (if (< x 0)
        -1
        1))

(define (make-rat n d)
    (let ((g (abs (gcd n d)))
          (s (signum (* n d))))
         (cons (* s (/ (abs n) g))
               (/ (abs d) g))))

(define numer car)
(define denom cdr)

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (add-rat x y)
    (make-rat
        (+ (* (numer x) (denom y))
           (* (numer y) (denom x)))
        (* (denom x)
           (denom y))))

(define (sub-rat x y)
    (make-rat
        (- (* (numer x) (denom y))
           (* (numer y) (denom x)))
        (* (denom x)
           (denom y))))

(define (mul-rat x y)
    (make-rat
        (* (numer x) (numer y))
        (* (denom x) (denom y))))

(define (div-rat x y)
    (make-rat
        (* (numer x) (denom y))
        (* (denom x) (numer y))))

(define (equal-rat? x y)
    (= (* (numer x) (denom y))
       (* (numer y) (denom x))))

(define one-half (make-rat 1 2))
(print-rat one-half)
;1/2

(define one-third (make-rat 1 3))
(print-rat
    (add-rat one-half one-third))
;5/6

(print-rat
    (mul-rat one-half one-third))
;1/6

(print-rat
    (add-rat one-third one-third))
    
(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

(define make-point cons)
(define x-point car)
(define y-point cdr)

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (average x y)
    (/ (+ x y) 2))

(define (midpoint-segment seg)
    (define (midpoint p1 p2)
        (make-point (average (x-point p1) (x-point p2))
                    (average (y-point p1) (y-point p2))))
    (midpoint (start-segment seg)
              (end-segment seg)))

(define seg (make-segment (make-point -2 -2)
                          (make-point 6 4)))

(print-point (midpoint-segment seg))

(define (segment-length seg)
    (sqrt (+ (square (- (x-point (start-segment seg))
                        (x-point (end-segment seg))))
             (square (- (y-point (start-segment seg))
                        (y-point (end-segment seg)))))))

(define (make-rectangle a-side b-side)
    (cons a-side b-side))
(define (rectangle-a-side rectangle)
    (car rectangle))
(define (rectangle-b-side rectangle)
    (cdr rectangle))

(define (area rectangle)
    (* (segment-length (rectangle-a-side rectangle))
       (segment-length (rectangle-b-side rectangle))))

(define (perimiter rectangle)
    (* 2 (+ (segment-length (rectangle-a-side rectangle))
            (segment-length (rectangle-b-side rectangle)))))

(define rect (make-rectangle (make-segment (make-point -2 -2)
                                           (make-point 6 -2))
                             (make-segment (make-point 6 -2)
                                           (make-point 6 4))))

(define (exp x y)
    (define (loop acc n)
        (if (= n 0)
            acc
            (loop (* acc x) (- n 1))))
    (loop 1 y))

(define (int-pair a b)
    (* (exp 2 a) (exp 3 b)))

(define (car-int-pair p)
    (div-int-pair p 2))
(define (cdr-int-pair p)
    (div-int-pair p 3))

(define (div-int-pair p x)
    (if (= (remainder p x) 0)
        (+ 1 (div-int-pair (/ p x) x))
        0))


(define (make-interval a b)
    (cons (min a b) (max a b)))
(define lower-bound car)
(define upper-bound cdr)

(define (add-interval x y)
  (make-interval (+ (lower-bound x) 
                    (lower-bound y))
                 (+ (upper-bound x) 
                    (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) 
               (lower-bound y)))
        (p2 (* (lower-bound x) 
               (upper-bound y)))
        (p3 (* (upper-bound x) 
               (lower-bound y)))
        (p4 (* (upper-bound x) 
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
    (if (spans-interval? y 0)
        (error "Divisor interval spans 0")
        (mul-interval x 
                (make-interval 
                 (/ 1.0 (upper-bound y)) 
                 (/ 1.0 (lower-bound y))))))

(define (sub-interval x y)
    (make-interval (- (lower-bound x) 
                      (upper-bound y))
                   (- (upper-bound x)
                      (lower-bound y))))

(define (width-interval x)
    (/ (- (upper-bound x)
          (lower-bound x)) 
       2))

(define (spans-interval? i x)
    (and (>= (upper-bound i) x)
         (<= (lower-bound i) x)))

(define (make-center-width c w)
    (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) 
        (upper-bound i)) 
     2))

(define (width i)
  (/ (- (upper-bound i) 
        (lower-bound i)) 
     2))

(define (make-center-percent c t)
    (make-center-width c (* c (/ t 100))))

(define (percent i)
    (* (/ (width i) (center i)) 100))