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

(define (contains-cycle? xs)
    (let ((visited ()))
        (define (iter xs)
            (cond ((null? xs) #f)
                  ((element-of? xs visited) #t)
                  (else (begin
                    (set! visited (cons xs visited))
                    (iter (cdr xs))))))
    (iter xs)))

(define (set-cddr! x v)
    (set-cdr! (cdr x) v))

(define (set-cadr! x v)
    (set-car! (cdr x) v))

(define (make-deque)
    (let ((front-ptr ()) (rear-ptr ()))
        (define (empty?)
            (null? front-ptr))

        (define (front)
            (if (empty?)
                (error "FRONT on empty deque")
                (car front-ptr)))

        (define (rear)
            (if (empty?)
                (error "REAR on empty deque") 
                (car rear-ptr)))

        (define (front-insert! item)
            (let ((new-node (cons item 
                                  (cons () ()))))
                (if (empty?)
                    (begin (set! front-ptr new-node)
                           (set! rear-ptr new-node))
                    (begin (set-cddr! new-node front-ptr)
                           (set-cadr! front-ptr new-node)
                           (set! front-ptr new-node)))))

        (define (rear-insert! item)
            (let ((new-node (cons item
                                  (cons () ()))))
                (if (empty?)
                    (begin (set! front-ptr new-node)
                           (set! rear-ptr new-node))
                    (begin (set-cadr! new-node rear-ptr)
                           (set-cddr! rear-ptr new-node)
                           (set! rear-ptr new-node)))))

        (define (front-delete!)
            (if (empty?)
                (error "FRONT-DELETE on empty deque")
                (let ((head (car front-ptr)))
                    (set! front-ptr (cddr front-ptr))
                    head)))

        (define (rear-delete!)
            (if (empty?)
                (error "REAR-DELETE on empty queue")
                (let ((last (car rear-ptr)))
                    (if (eq? front-ptr rear-ptr)
                        (set! front-ptr ()))
                    (set! rear-ptr (cadr rear-ptr))
                    last)))

        (define (dispatch m . args)
            (cond ((eq? m 'empty?) (empty?))
                  ((eq? m 'front) (front))
                  ((eq? m 'rear) (rear))
                  ((eq? m 'front-insert!) (apply front-insert! args))
                  ((eq? m 'rear-insert!) (apply rear-insert! args))
                  ((eq? m 'front-delete!) (front-delete!))
                  ((eq? m 'rear-delete!) (rear-delete!))
                  (else (error "Unsupported deque message" m))))

        dispatch))

(define (empty-deque? d) (d 'empty?))
(define (front-deque d) (d 'front))
(define (rear-deque d) (d 'rear))
(define (front-insert-deque! d item) (d 'front-insert! item))
(define (rear-insert-deque! d item) (d 'rear-insert! item))
(define (front-delete-deque! d) (d 'front-delete!))
(define (rear-delete-deque! d) (d 'rear-delete!))

(define (print-deque! d)
    (if (not (empty-deque? d))
        (begin
            (display (front-delete-deque! d))
            (newline)
            (print-deque! d))))