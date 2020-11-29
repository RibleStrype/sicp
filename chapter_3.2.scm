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


(define (make-queue)
    (let ((front-ptr ()) (rear-ptr ()))
        (define (empty?)
            (null? front-ptr))

        (define (front)
            (if (empty?)
                (error "FRONT on empty queue")
                (car front-ptr)))

        (define (insert! item)
            (let ((new-pair (cons item ())))
                (begin
                    (if (empty?)
                        (set! front-ptr new-pair)
                        (set-cdr! rear-ptr new-pair))
                    (set! rear-ptr new-pair))))

        (define (delete!)
            (if (empty?)
                (error "DELETE on empty queue")
                (let ((head (car front-ptr)))
                    (set! front-ptr (cdr front-ptr))
                    head)))

        (define (dispatch m . args)
            (cond ((eq? m 'empty?) (empty?))
                  ((eq? m 'front) (front))
                  ((eq? m 'insert!) (apply insert! args))
                  ((eq? m 'delete!) (delete!))
                  (else (error "Unsupported queue message" m))))

        dispatch))

(define (empty-queue? q) (q 'empty?))
(define (front-queue q) (q 'front))
(define (insert-queue! q item) (q 'insert! item))
(define (delete-queue! q) (q 'delete!))
(define (print-queue! q)
    (if (not (empty-queue? q))
        (begin
            (display (delete-queue! q))
            (newline)
            (print-queue! q))))