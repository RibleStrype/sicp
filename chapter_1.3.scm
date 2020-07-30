(define (filtered-accum filter combiner null-value term a next b)
    (define (iter a result)
        (cond ((> a b) result)
              ((filter a) (iter (next a) (combiner result (term a))))
              (else (iter (next a) result))))

    (iter a null-value))

(define (accum-iter combiner null-value term a next b)
    (define (always-true x) #t)

    (filtered-accum always-true combiner null-value term a next b))

(define (accum-recur combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner (term a)
                  (accum-recur combiner null-value term (next a) next b))))

(define (sum term a next b)
    (accum-iter + 0 term a next b))

(define (identity x) x)

(define (inc x) (+ x 1))

(define (sum-num a b)
    (sum identity a inc b))

(define (simpson f a b n)
    (define h (/ (- b a) n))

    (define (y k)
        (f (+ a (* k h))))

    (define (quot k)
        (cond ((or (= k 0) (= k n)) 1)
              ((even? k) 2)
              (else 4)))
    
    (define (term k)
        (* (quot k) (y k)))

    (* (/ h 3) (sum term 0 inc n)))

(define (product term a next b)
    (accum-iter * 1 term a next b))

(define (factorial n)
    (product identity 1 inc n))

(define (approx-pi n)
    (define (term i)
        (* (/ i (- i 1)) (/ i (+ i 1))))

    (define (inc2 x)
        (+ x 2))

    (* (/ 8.0 3) (product term 4 inc2 n)))

(define (sum-square-prime a b)
    (filtered-accum prime? + 0 square a inc b))

(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))

(define (prod-relative-prime n)
    (define (relative-prime? i)
        (= (gcd i n) 1))

    (filtered-accum relative-prime? * 1 identity 1 inc n))