(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (zero? x) (apply-generic 'zero? x))
(define (exp x y) (apply-generic 'exp x y))

(define (put op types item table)
    (cons (list op types item) table))

(define (get op type)
    (let ((res (find (lambda (entry)
                        (and (eq? (car entry) op)
                             (equal? (cadr entry) type)))
                     table)))
        (if res
            (caddr res)
            res)))

(define (put-coercion from to coercion table)
    (put 'coerce (list from to) coercion table))

(define (get-coercion from to)
    (get 'coerce (list from to)))

(define (attach-tag tag x)
    (if (number? x)
        x
        (list tag x)))

(define (type-tag x)
    (if (number? x)
        'scheme-number
        (car x)))

(define (contents x)
    (if (number? x)
        x
        (cadr x)))

(define (apply-generic op . args)
    (let* ((type-tags (map type-tag args))
           (proc (get op type-tags)))
          (if proc
              (apply proc (map contents args))
              (if (= (length args) 2)
                  (let* ((t1 (car type-tags))
                        (t2 (cadr type-tags))
                        (a1 (car args))
                        (a2 (cadr args))
                        (t1->t2 (get-coercion t1 t2))
                        (t2->t1 (get-coercion t2 t1)))
                    (cond (t1->t2 
                            (apply-generic op (t1->t2 a1) a2))
                          (t2->t1
                            (apply-generic op a1 (t2->t1 a2)))
                          (else (error
                                    "No method for these types"
                                    (list op type-tags)))))
                  (error 
                    "No method for these types"
                    (list op type-tags))))))

(define (install-scheme-number-package table)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y)))
       (put 'sub '(scheme-number scheme-number)
            (lambda (x y) (tag (- x y)))
            (put 'mul '(scheme-number scheme-number)
                 (lambda (x y) (tag (* x y)))
                 (put 'div '(scheme-number scheme-number)
                      (lambda (x y) (tag (/ x y)))
                      (put 'make 'scheme-number
                           (lambda (x) (tag x))
                           (put 'equ? '(scheme-number scheme-number)
                                =
                                (put 'zero? '(scheme-number)
                                     (lambda (x) (= x 0))
                                     (put-coercion 'scheme-number 'rational
                                                   (lambda (x) (make-rational x 1))
                                                   (put 'exp '(scheme-number scheme-number)
                                                        (lambda (x y) (tag (expt x y))) 
                                                        (put-coercion 'scheme-number 'complex
                                                                      (lambda (x) (make-complex-from-real-imag x 0))
                                                                      table)))))))))))

(define (install-rational-package table)
    (define numer car)
    (define denom cdr)
    (define (make-rat n d)
        (let ((g (gcd n d)))
            (cons (/ n g) (/ d g))))

    (define (add-rat x y)
        (make-rat (+ (* (numer x) (denom y))
                     (* (numer y) (denom x)))
                  (* (denom x) (denom y))))

    (define (sub-rat x y)
        (make-rat (- (* (numer x) (denom y))
                     (* (numer y) (denom x)))
                  (* (denom x) (denom y))))

    (define (mul-rat x y)
        (make-rat (* (numer x) (numer y))
                  (* (denom x) (denom y))))

    (define (div-rat x y)
        (make-rat (* (numer x) (denom y))
                  (* (denom x) (numer y)))) 

    (define (eq-rat x y)
        (= (* (numer x) (denom y))
           (* (denom x) (numer y))))

    (define (zero? x)
        (= 0 (numer x)))

    (define (put-binary op proc table)
        (put op '(rational rational)
             (lambda (x y) (tag (proc x y)))
             table))

    (define (tag x)
        (attach-tag 'rational x))

    (put-binary 'add add-rat
         (put-binary 'sub sub-rat
              (put-binary 'mul mul-rat 
                          (put-binary 'div div-rat 
                                      (put 'make 'rational
                                           (lambda (n d) (tag (make-rat n d)))
                                           (put 'equ? '(rational rational)
                                                eq-rat
                                                (put 'zero? '(rational)
                                                     zero?
                                                     table))))))))

(define (install-rectangular-package table)
    (define (make-from-real-imag x y)
        (cons x y))
    (define real-part car)
    (define imag-part cdr)
    (define (magnitude z)
        (sqrt (+ (square (real-part z))
                 (square (imag-part z)))))
    (define (angle z)
        (atan (real-part z) (imag-part z)))
    (define (make-from-mag-ang r a)
        (cons (* r (cos a)) (* r (sin a))))

    (define (tag z)
        (attach-tag 'rectangular z))

    (put 'real-part '(rectangular) real-part
        (put 'imag-part '(rectangular) imag-part
            (put 'magnitude '(rectangular) magnitude
                (put 'angle '(rectangular) angle
                    (put 'make-from-real-imag 'rectangular
                         (lambda (x y) (tag (make-from-real-imag x y)))
                         (put 'make-from-mag-ang 'rectangular
                              (lambda (r a) (tag make-from-mag-ang r a))
                              table)))))))

(define (install-polar-package table)
    (define magnitude car)
    (define angle cdr)
    (define (make-from-mag-ang r a) (cons r a))
    (define (real-part z)
        (* (magnitude z) (cos (angle z))))
    (define (imag-part z)
        (* (magnitude z) (sin (angle z))))
    (define (make-from-real-imag x y)
        (cons (sqrt (+ (square x) (square y)))
              (atan x y)))
    
    (define (tag z)
        (attach-tag 'polar z))

    (put 'real-part '(polar) real-part
        (put 'imag-part '(polar) imag-part
            (put 'magnitude '(polar) magnitude
                (put 'angle '(polar) angle
                    (put 'make-from-real-imag 'polar
                         (lambda (x y) (tag (make-from-real-imag x y)))
                         (put 'make-from-mag-ang 'polar
                              (lambda (r a) (tag (make-from-mag-ang r a)))
                              table)))))))

(define (install-complex-package table)
    (define (make-from-real-imag x y)
        ((get 'make-from-real-imag
              'rectangular)
         x y))

    (define (make-from-mag-ang r a)
        ((get 'make-from-mag-ang
              'polar)
         r a))

    (define (tag z)
        (attach-tag 'complex z))

    (define (magnitude z)
        (apply-generic 'magnitude z))
    (define (angle z)
        (apply-generic 'angle z))
    (define (real-part z)
        (apply-generic 'real-part z))
    (define (imag-part z)
        (apply-generic 'imag-part z))

    (define (equ? x y)
        (and (= (real-part x) (real-part y))
             (= (imag-part x) (imag-part y))))

    (define (zero? z)
        (and (= 0 (real-part z))
             (= 0 (imag-part z))))

    (define (add-complex x y)
        (make-from-real-imag
            (+ (real-part x) (real-part y)) 
            (+ (imag-part x) (imag-part y))))

    (put 'make-from-real-imag 'complex
         (lambda (x y) (tag (make-from-real-imag x y)))
         (put 'make-from-mag-ang 'complex
              (lambda (r a) (tag (make-from-mag-ang r a)))
              (put 'magnitude '(complex)
                    magnitude
                    (put 'angle '(complex)
                         angle
                         (put 'equ? '(complex complex)
                            equ?
                            (put 'zero? '(complex)
                                 zero?
                                 (install-rectangular-package
                                    (install-polar-package 
                                        (put 'add '(complex complex)
                                             (lambda (x y) (tag (add-complex x y)))
                                             table))))))))))

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))
(define (make-rational n d)
  ((get 'make 'rational) n d))
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (magnitude z)
    (apply-generic 'magnitude z))
(define (angle z)
    (apply-generic 'angle z))

(define table 
        (install-scheme-number-package
            (install-rational-package 
                (install-complex-package ()))))