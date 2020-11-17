(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define z1
    (let ((x (list 'a 'b)))
        (cons x x)))

(define z2
    (let* ((x (list 'b))
           (y (cons 'a x)))
        (cons x y)))

(define z3
    (let* ((x (list 'b))
           (y (cons x x)))
        (cons y y)))

(define z4 (make-cycle (list 'a 'b 'c)))

(define (element-of? x xs)
    (cond ((null? xs) #f)
          ((eq? x (car xs)) #t)
          (else (element-of? x (cdr xs)))))

(define (count-pairs2 x)
    (let ((visited ()))
        (define (iter p)
            (cond ((not (pair? p)) 0)
                  ((element-of? p visited) 0)
                  (else (begin
                    (set! visited (cons p visited))
                    (+ (iter (car p))
                       (iter (cdr p))
                       1)))))
        (iter x)))