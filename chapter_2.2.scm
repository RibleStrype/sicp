(define (last-pair xs)
    (cond ((null? xs) xs)
          ((null? (cdr xs)) (list (car xs)))
          (else (last-pair (cdr xs)))))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) 
            (append (cdr list1) 
                    list2))))

(define (reverse xs)
    (if (null? xs)
        xs
        (append (reverse (cdr xs)) (list (car xs)))))

(define (cc amount coin-values)
  (define no-more? null?)
  (define first-denomination car)
  (define except-first-denomination cdr)
  (cond ((= amount 0) 
         1)
        ((or (< amount 0) 
             (no-more? coin-values)) 
         0)
        (else
         (+ (cc 
             amount
             (except-first-denomination 
              coin-values))
            (cc 
             (- amount
                (first-denomination 
                 coin-values))
             coin-values)))))

(define (same-parity x . xs)
    (define (loop xs)
        (cond ((null? xs) xs)
              ((= (remainder x 2) (remainder (car xs) 2)) (cons (car xs) (loop (cdr xs))))
              (else (loop (cdr xs)))))
    (cons x (loop xs)))