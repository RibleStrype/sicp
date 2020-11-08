(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (zero? x) (apply-generic 'zero? x))
(define (exp x y) (apply-generic 'exp x y))
(define (square x) (mul x x))
(define (square-root x) (apply-generic 'sqrt x))
(define (arctan x y) (apply-generic 'arctan x y))
(define (sine x) (apply-generic 'sine x))
(define (cosine x) (apply-generic 'cosine x))
(define (negate x) (apply-generic 'negate x))

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

(define type-hierarchy '(integer rational real complex))

(define (super-type type)
    (let ((super-types 
            (cdr (drop-while (lambda (t) (not (eq? t type))) type-hierarchy))))
        (if (null? super-types)
            #f
            (car super-types))))

(define (sub-type type)
    (let ((sub-types (take-while (lambda (t) (not (eq? t type))) type-hierarchy)))
        (if (null? sub-types)
            #f
            (last sub-types))))

; is x sub-type of y?
(define (sub-type? x y)
    (let ((super-types 
            (drop-while 
                (lambda (t)
                    (not (eq? t x))) 
                type-hierarchy)))
        (if (find (lambda (t) (eq? t y)) (cdr super-types))
            #t
            #f)))

(define (put-raise from-type raise table)
    (let ((to-type (super-type from-type)))
        (if to-type
            (put 'raise (list from-type to-type) raise table)
            table)))

(define (get-raise from)
    (get 'raise (list from (super-type from))))

(define (put-project from-type project table)
    (let ((to-type (sub-type from-type)))
        (if to-type
            (put 'project (list from-type to-type) project table)
            table)))

(define (get-project from-type)
    (let ((to-type (sub-type from-type)))
        (if to-type
            (get 'project (list from-type to-type))
            #f)))

(define (attach-tag tag x)
    (if (number? x)
        x
        (list tag x)))

(define (type-tag x)
    (cond ((number? x)
            (if (integer? x)
                'integer
                'real))
          ((pair? x) (car x))
          (else #f)))

(define (contents x)
    (cond ((number? x) x)
          ((pair? x) (cadr x))
          (else #f)))

(define (drop-num x)
    (let ((type (type-tag x)))
        (if type
            (let ((project (get-project type)))
                (if project
                    (let ((y (project (contents x))))
                        (let ((raise (get-raise (type-tag y))))
                            (if (equ? (raise (contents y)) x)
                                (drop-num y)
                                x)))
                    x))
            x)))
    
(define (apply-generic op . args)
    (define (coerce to-type)
        (lambda (arg)
            (let ((from-type (type-tag arg)))
                (if (sub-type? from-type to-type)
                    (let ((raise (get-raise from-type)))
                        (if raise
                            ((coerce to-type) (raise (contents arg)))
                            arg))
                    arg))))

    (define (get-proc args)
        (let ((type-tags (map type-tag args)))
            (get op type-tags)))

    (define (coerced-proc to-type)
        (let* ((coerced-args (map (coerce to-type) args))
              (proc (get-proc coerced-args)))
             (if proc
                 (lambda () (apply proc (map contents coerced-args))) 
                 #f)))

    (let ((proc (get-proc args)))
        (if proc
            (drop-num (apply proc (map contents args)))
            (let ((proc2 (fold-left 
                            (lambda (res type)
                                (or res (coerced-proc type))) 
                            #f 
                            (map type-tag args))))
                (if proc2
                    (drop-num (proc2))
                    (error
                        "No method for theses types"
                        (list op (map type-tag args))))))))

(define (install-scheme-number-package table)

    (define (install type-tag table)
        (define (tag x)
            (attach-tag type-tag x))

        (put 'add (list type-tag type-tag)
             (lambda (x y) (tag (+ x y)))
             (put 'sub (list type-tag type-tag)
                  (lambda (x y) (tag (- x y)))
                  (put 'mul (list type-tag type-tag)
                       (lambda (x y) (tag (* x y)))
                       (put 'div (list type-tag type-tag)
                            (lambda (x y) (tag (/ x y)))
                            (put 'make type-tag
                                 (lambda (x) (tag x))
                                 (put 'equ? (list type-tag type-tag)
                                      =
                                      (put 'zero? (list type-tag)
                                           (lambda (x) (= x 0))
                                           (put 'exp (list type-tag type-tag)
                                                (lambda (x y) (tag (expt x y)))
                                                (put 'sqrt (list type-tag)
                                                     sqrt
                                                     (put 'arctan (list type-tag type-tag)
                                                          atan
                                                          (put 'cosine (list type-tag)
                                                               cos
                                                               table))))))))))))

    (install 'integer 
             (install 'real 
                      (put-raise 'integer
                                 (lambda (x) (make-rational x 1))
                                 (put-raise 'real
                                            (lambda (x) (make-complex-from-real-imag x 0))
                                            table)))))

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

    (define (square-root x)
        (make-rat (sqrt (numer x))
                  (sqrt (denom x))))

    (define (arctan x y)
        (let ((common-denom (* (denom x)
                               (denom y))))
            (atan (* (numer x) (/ common-denom (denom x)))
                  (* (numer y) (/ common-denom (denom y))))))

    (define (cosine x)
        (cos (/ (numer x) (denom x))))

    (define (sine x)
        (sin (/ (numer x) (denom x))))

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
                                                     (put-raise 'rational
                                                          (lambda (x) (/ (numer x) (denom x))) 
                                                          (put-project 'rational
                                                                       numer
                                                                       (put 'sqrt '(rational)
                                                                            square-root
                                                                            (put-binary 'arctan
                                                                                        arctan
                                                                                        (put 'cosine '(rational)
                                                                                             cosine
                                                                                             (put 'sine '(rational)
                                                                                                  sine
                                                                                                  table))))))))))))))

(define (install-rectangular-package table)
    (define (make-from-real-imag x y)
        (cons x y))
    (define real-part car)
    (define imag-part cdr)
    (define (magnitude z)
        (square-root (add (square (real-part z))
                          (square (imag-part z)))))
    (define (angle z)
        (arctan (real-part z) (imag-part z)))
    (define (make-from-mag-ang r a)
        (cons (mul r (cos a)) (mul r (sin a))))

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
        (mul (magnitude z) (cosine (angle z))))
    (define (imag-part z)
        (mul (magnitude z) (sine (angle z))))
    (define (make-from-real-imag x y)
        (cons (square-root (add (square x) (square y)))
              (arctan x y)))
    
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
                                             (put-project 'complex
                                                          (lambda (z)
                                                            (make-rational (real-part z) 1))
                                                          (put 'real-part '(complex)
                                                               real-part
                                                               (put 'imag-part '(complex)
                                                                    imag-part
                                                                    table)))))))))))))

(define variable? symbol?)
(define (same-variable? x y)
    (and (variable? x)
         (variable? y)
         (eq? x y)))
(define (make-term order coeff)
        (list order coeff))
(define order car)
(define coeff cadr)

(define (install-sparse-package table)
    (define (tag terms) (attach-tag 'sparse terms))

    (define empty-termlist? null?) 
    (define first-term car)
    (define rest-terms cdr)

    (define (adjoin-term t L)
        (cons t L))

    (define the-empty-termlist (tag ()))

    (put 'make-term-list 'sparse
         tag
         (put 'empty-termlist? '(sparse)
              empty-termlist?
              (put 'first-term '(sparse)
                   first-term
                   (put 'rest-terms '(sparse)
                        (lambda (L) (tag (rest-terms L))) 
                        (put 'adjoin-term '(sparse sparse)
                             (lambda (t L) (tag (adjoin-term t L)))
                             (put 'the-empty-termlist '(sparse)
                                  (lambda (L) the-empty-termlist)
                                  table)))))))

(define (install-dense-package table)
    (define (tag terms)
        (attach-tag 'dense terms))
        
    (define (empty-termlist? L) (null? L))

    (define (first-term L)
        (make-term (length (cdr L))
                   (car L)))

    (define (rest-terms L)
        (drop-while zero? (cdr L)))

    (define (pad n L)
        (if (zero? n)
            L
            (cons 0 
                  (pad (- n 1) L))))

    (define (adjoin-term t L)
        (cons (coeff t)
              (pad (sub (order t) (length L)) 
                   L)))

    (define the-empty-termlist (tag ()))

    (put 'make-term-list 'dense
         tag
         (put 'empty-termlist? '(dense)
              empty-termlist?
              (put 'first-term '(dense)
                   first-term
                   (put 'rest-terms '(dense)
                        (lambda (L) (tag (rest-terms L)))
                        (put 'adjoin-term '(dense dense)
                             (lambda (t L) (tag (adjoin-term t L)))
                             (put 'the-empty-termlist '(dense)
                                  (lambda (L) the-empty-termlist)
                                  table)))))))

(define (install-polynomial-package table)
    (define (make-sparse variable term-list)
        (make-poly variable
                   ((get 'make-term-list 'sparse) term-list)))

    (define (make-dense variable term-list)
        (make-poly variable
                   ((get 'make-term-list 'dense) term-list)))

    (define (make-poly variable term-list)
        (cons variable term-list))

    (define (variable p) (car p))
    (define (term-list p) (cdr p))

    (define (add-poly p1 p2)
        (if (same-variable? (variable p1)
                            (variable p2))
            (make-poly
                (variable p1)
                (add-terms (term-list p1)
                           (term-list p2)))
            (error "Polys not in same var: ADD-POLY" 
                   (list p1 p2))))

    (define (mul-poly p1 p2)
        (if (same-variable? (variable p1)
                            (variable p2))
            (make-poly
                (variable p1)
                (mul-terms (term-list p1)
                           (term-list p2)))
            (error "Polys not in same var: MUL-POLY"
                   (list p1 p2))))

    (define (sub-poly p1 p2)
        (add-poly p1 (negate-poly p2)))

    (define (negate-poly p)
        (mul-poly p
                  (make-poly (variable p)
                             (adjoin-term (make-term 0 -1) (the-empty-termlist p)))))

    (define (add-terms L1 L2)
        (cond ((empty-termlist? L1) L2)
              ((empty-termlist? L2) L1)
              (else
                (let* ((t1 (first-term L1))
                       (t2 (first-term L2)))
                    (cond ((> (order t1) (order t2)) 
                            (adjoin-term
                                t1
                                (add-terms (rest-terms L1)
                                           L2)))
                          ((< (order t1) (order t2)) 
                            (adjoin-term
                                t2
                                (add-terms L1
                                           (rest-terms L2))))
                          (else 
                                (adjoin-term
                                    (make-term (order t1)
                                               (add (coeff t1)
                                                    (coeff t2)))
                                    (add-terms (rest-terms L1)
                                               (rest-terms L2)))))))))

    (define (mul-terms L1 L2)
        (if (empty-termlist? L1)
            L1
            (add-terms
                (mul-term-by-all-terms
                    (first-term L1)
                    L2)
                (mul-terms
                    (rest-terms L1)
                    L2))))

    (define (mul-term t1 t2)
        (make-term (add (order t1) (order t2))
                   (mul (coeff t1) (coeff t2))))

    (define (mul-term-by-all-terms t L)
        (if (empty-termlist? L)
            L
            (adjoin-term
                (mul-term t (first-term L))
                (mul-term-by-all-terms t (rest-terms L)))))

    (define (tag p) (attach-tag 'polynomial p))

    (define (the-empty-termlist p)
        (apply-generic 'the-empty-termlist (term-list p)))
    (define (empty-termlist? L) 
        (apply-generic 'empty-termlist? L))
    (define (first-term L)
        (apply-generic 'first-term L))
    (define (rest-terms L)
        (apply-generic 'rest-terms L))
    (define (adjoin-term t L)
        (if (zero? (coeff t))
            L
            (apply-generic 'adjoin-term
                           (attach-tag (type-tag L) t)
                           L)))

    (put 'add '(polynomial polynomial)
         (lambda (p1 p2)
            (tag (add-poly p1 p2)))
         (put 'mul '(polynomial polynomial)
              (lambda (p1 p2)
                (tag (mul-poly p1 p2)))
              (put 'make-sparse 'polynomial
                   (lambda (variable terms) (tag (make-sparse variable terms)))
                   (put 'zero? '(polynomial)
                        (lambda (p) (empty-termlist? (term-list p)))
                        (put 'sub '(polynomial polynomial)
                             (lambda (p1 p2)
                                (tag (sub-poly p1 p2)))
                             (put 'negate '(polynomial)
                                (lambda (p) (tag (negate-poly p)))
                                (put 'make-dense 'polynomial
                                     (lambda (variable terms) (tag (make-dense variable terms)))
                                     (install-sparse-package
                                        (install-dense-package table))))))))))

(define (make-rational n d)
  ((get 'make 'rational) n d))
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))
(define (make-polynomial-sparse var terms)
    ((get 'make-sparse 'polynomial) var terms))
(define (make-polynomial-dense var terms)
    ((get 'make-dense 'polynomial) var terms))

(define (real-part z)
    (apply-generic 'real-part z))
(define (imag-part z)
    (apply-generic 'imag-part z))
(define (magnitude z)
    (apply-generic 'magnitude z))
(define (angle z)
    (apply-generic 'angle z))

(define table 
        (install-scheme-number-package
            (install-rational-package 
                (install-complex-package 
                    (install-polynomial-package ())))))

(define x (make-polynomial-sparse 'x (list (list 2 1) (list 1 5) (list 0 7))))
(define y (make-polynomial-dense 'x (list 1 5 7)))