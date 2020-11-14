(define (make-unprotected-account balance)
    (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
                   balance)
            "Insufficient funds"))

    (define (deposit amount)
        (begin (set! balance (+ balance amount))
               balance))           

    (define (dispatch m)
        (cond ((eq? 'withdraw m) withdraw)
              ((eq? 'deposit m) deposit)
              (else (error "Unknown request: MAKE-ACCOUNT" m))))

    dispatch)

(define (protect account password)
    (let ((attempts 0))
        (define (incorrect-password _)
            (if (> attempts 6)
                "I'm calling the cops"
                "Incorrect password"))

        (lambda (pwd m)
            (if (eq? pwd password)
                (begin (set! attempts 0)
                       (account m))
                (begin (set! attempts (+ attempts 1))
                       incorrect-password)))))

(define (make-account balance password)
    (protect (make-unprotected-account balance)
             password))

(define (make-joint account orig-pwd new-pwd)
    (protect (lambda (m) (account orig-pwd m))
             new-pwd))

(define peter-acc 
  (make-account 100 'open-sesame))

(define paul-acc
  (make-joint peter-acc 
              'open-sesame 
              'rosebud))

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

(define (monte-carlo trials experiment)

    (define (iter remaining passed)
        (cond ((= remaining 0) (/ passed trials))
              ((experiment) (iter (- remaining 1)
                                  (+ passed 1)))
              (else (iter (- remaining 1)
                          passed))))

    (iter trials 0))

(define (estimate-integral p x1 x2 y1 y2 trials)

    (define (random-in-range low high)
        (let ((range (- high low)))
            (+ low (random range))))

    (define (experiment)
        (let ((x (random-in-range (min x1 x2) 
                                  (max x1 x2)))
              (y (random-in-range (min y1 y2) 
                                  (max y1 y2))))
            (p x y)))

    (monte-carlo trials experiment))

(define (estimate-pi trials)
    (* 4.0
       (estimate-integral
           (lambda (x y)
            (< (+ (square x) (square y)) 
               1))
           -1.0 1.0 -1.0 1.0
           trials)))