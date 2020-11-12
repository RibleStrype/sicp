(define (make-account balance password)
    (let ((consequent-login-attempts 0))
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

        (define (dispatch-login pwd m)
            (if (eq? pwd password)
                (begin (set! consequent-login-attempts 0)
                       (dispatch m))
                (begin (set! consequent-login-attempts 
                             (+ consequent-login-attempts 1))
                       (lambda (_)
                           (if (> consequent-login-attempts 6)
                               "I'm calling the police"
                               "Incorrect password")))))

        dispatch-login))

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