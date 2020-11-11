(define (make-account balance)
    (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
                    balance)
            "Insufficient funds"))

    (define (deposit amount)
        (begin 
            (set! balance (+ balance amount))
            balance))

    (define (dispatch m)
        (cond ((eq? 'withdraw m) withdraw)
              ((eq? 'deposit m) deposit)
              (else (error "Unknown request: MAKE-ACCOUNT" m))))
    
    dispatch)

(define (make-accumulator sum)
    (lambda (n)
        (begin (set! sum (+ sum n))
               sum)))

(define (make-monitored f)
    (let ((count 0))
        (lambda (x)
            (cond ((eq? 'how-many-calls? x) count)
                  ((eq? 'reset-count x) (set! count 0))
                  (else (begin (set! count (+ count 1))
                               (f x)))))))