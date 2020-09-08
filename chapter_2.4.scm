(define (install-sum-package table)
    (define (deriv-sum args var)
        (make-sum (deriv (addend args) var)
                  (deriv (augend args) var)))
    
    (define addend car)
    (define (augend args)
        (fold-left make-sum 0 (cdr args)))

    (put 'deriv '(+) deriv-sum table))

(define (make-sum x y)
    (cond ((=number? x 0) y)
          ((=number? y 0) x)
          ((and
            (number? x)
            (number? y)) (+ x y))
          (else (list '+ x y))))

(define (make-product x y)
    (cond ((=number? x 0) 0)
          ((=number? y 0) 0)
          ((=number? x 1) y)
          ((=number? y 1) x)
          ((and
            (number? x)
            (number? y)) (* x y))
          (else (list '* x y))))

(define (install-product-package table)
    (define (deriv-product args var)
        (make-sum (make-product (multiplier args)
                                (deriv (multiplicand args) var))
                  (make-product (deriv (multiplier args) var)
                                (multiplicand args))))
    
    (define multiplier car)
    (define (multiplicand args)
        (fold-left make-product 1 (cdr args)))

    (put 'deriv '(*) deriv-product table))

(define (install-exponentiation-package table)
    (define (deriv-exp args var)
        (let* ((b (base args))
               (p (power args)))
                (make-product
                    p
                    (make-product
                        (make-exponentiation b (make-sum p -1))
                        (deriv b var)))))

    (define (make-exponentiation base power)
      (cond ((=number? power 0) 1)
            ((=number? power 1) base)
            (else (list '** base power))))

    (define base car)
    (define power cadr)

    (put 'deriv '(**) deriv-exp table))

(define (=number? expr n)
    (and
        (number? expr)
        (= expr n)))

(define (put op type item table)
    (cons (list (list op type) item) table))

(define (get op type table)
    (define (contains? x list)
        (cond ((null? list) #f)
              ((eq? x (car list)) #t)
              (else (contains? x (cdr list)))))

    (define (same-key? key)
        (and (eq? op (car key))
             (contains? type (cadr key))))

    (cond ((null? table) #f)
          ((same-key? (caar table)) (cadr (car table)))
          (else (get op type (cdr table)))))

(define variable? symbol?)
(define (same-variable? x y)
    (and (variable? x)
         (variable? y)
         (eq? x y)))

(define deriv
    (let ((table (install-sum-package 
                    (install-product-package 
                        (install-exponentiation-package ())))))
        (lambda (exp var) 
            (cond ((number? exp) 0)
                  ((variable? exp)
                    (if (same-variable? var exp)
                        1
                        0))
                  (else ((get 'deriv (operator exp) table)
                         (operands exp)
                         var))))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (get-record division name)
    (let ((get-name (get 'get-name division table)))
          (find
            (lambda (record) (eq? name (get-name record)))
            (get 'records division table))))

(define (get-salary division name)
    ((get 'get-salary division table)
        (get-record division name)))

(define (find-employee-record name divisions)
    (fold-left
        (lambda (res division)
            (or res (get-record division name)))
        #f
        divisions))
    
(define (install-marketing table)
    (define (make-record name salary address)
        (list name salary address))

    (define records 
            (list 
                (make-record 'John 75000 "Oxford street")
                (make-record 'Bob 80000 "California")))

    (define get-name car)
    (define get-salary cadr)

    (put 'records '(marketing) records
        (put 'get-name '(marketing) get-name
            (put 'get-salary '(marketing) get-salary table))))

(define (install-infra table)
    (define (make-record name salary address)
        (list
            (list 'name name)
            (list 'salary salary)
            (list 'address address)))

    (define records 
        (list 
            (make-record 'Vasja 20000 "Under the bridge")
            (make-record 'John 120000 "Mansion")))
    
    (define (project-field name)
        (lambda (record)
            (cadr
                (find
                    (lambda (field)
                        (eq? name (car field))) 
                    record))))

    (define get-name (project-field 'name))
    (define get-salary (project-field 'salary))

    (put 'records '(infra) records
        (put 'get-name '(infra) get-name
            (put 'get-salary '(infra) get-salary table))))

(define table (install-marketing (install-infra ())))