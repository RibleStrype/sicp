(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(define (f-recur n)
  (if (< n 3)
    n
    (+
      (f-recur (- n 1))
      (* 2 (f-recur (- n 2)))
      (* 3 (f-recur (- n 3))))))


(define (f-iter n)
  (define (loop a b c n)
    (cond ((< n 2) n)
      ((= n 2) c)
      (else (loop b c (+ c (* 2 b) (* 3 a)) (- n 1)))))
    
  (loop 0 1 2 n))

(define (pascal row col)
  (cond 
    ((or (< col 1) (< row 1)) 0)
    ((or (= col 1) (= col row)) 1)
    (else (+ (pascal (- row 1) (- col 1)) (pascal (- row 1) col)))))

(define (count-change amount)

  (define (loop amount n)
    (cond ((= amount 0) 1)
      ((< amount 0) 0)
      ((= n 0) 0)
      (else (+ (loop amount (- n 1))
               (loop (- amount (first-denom n)) n)))))

  (define (first-denom n)
    (cond ((= n 5) 50)
      ((= n 4) 25)
      ((= n 3) 10)
      ((= n 2) 5)
      ((= n 1) 1)))

  (loop amount 5))

(define (fast-exp b n)

  (define (loop a b n)
    (cond ((= n 0) a)
      ((even? n) (loop a (square b) (/ n 2)))
      (else (loop (* a b) b (- n 1)))))

  (loop 1 b n))

  (define (fast-mult b n)
  
    (define (loop a b n)
      (cond ((= n 0) a)
            ((even? n) (loop a (double b) (halve n)))
            (else (loop (+ a b) b (- n 1)))))
    
    (define (halve x)
      (/ x 2))

    (define (double x)
      (* x 2))

    (loop 0 b n))


(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) 
         n)
        ((divides? test-divisor n) 
         test-divisor)
        (else (find-divisor 
               n 
               (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder 
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder 
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) 
         (fast-prime? n (- times 1)))
        (else false)))

(define (congr-test n)
  (define (loop a)
    (if (or (<= a 0) (not (= (expmod a n n) a)))
      a
      (loop (- a 1))))
    
  (loop (- n 1)))