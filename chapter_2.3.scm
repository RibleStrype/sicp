(define (memq item x) (cond ((null? x) false) ((eq? item (car x)) x)
         (else (memq item (cdr x)))))

(define (equal? a b)
    (if (and (pair? a) (pair? b))
        (and (eq? (car a) (car b))
             (equal? (cdr a) (cdr b)))
        (eq? a b)))

(define (deriv exp var)
    (cond ((number? exp) 0)
          ((variable? exp)
            (if (same-variable? exp var) 1 0))
          ((sum? exp)
            (make-sum (deriv (addend exp) var) 
                      (deriv (augend exp) var)))
          ((product? exp)
            (make-sum
                (make-product
                    (multiplier exp)
                    (deriv (multiplicand exp) var)) 
                (make-product
                    (deriv (multiplier exp) var)
                    (multiplicand exp))))
          ((exponentiation? exp)
            (make-product
              (exponent exp)
              (make-product
                (make-exponentiation
                  (base exp)
                  (make-sum (exponent exp) -1))
                (deriv (base exp) var))))
          (else (error "unknown expression type: DERIV" exp))))

(define variable? symbol?)
(define (same-variable? x y)
    (and
        (variable? x)
        (variable? y)
        (eq? x y)))

(define (=number? expr n)
    (and
        (number? expr)
        (= expr n)))

(define (make-sum x y)
    (cond ((=number? x 0) y)
          ((=number? y 0) x)
          ((and
            (number? x)
            (number? y)) (+ x y))
          (else (list '+ x y))))
(define (sum? exp) (eq? '+ (car exp)))
(define addend cadr)
(define augend caddr)
(define (augend expr)
  (fold-left
    make-sum
    0
    (cddr expr)))

(define (make-product x y)
    (cond ((=number? x 0) 0)
          ((=number? y 0) 0)
          ((=number? x 1) y)
          ((=number? y 1) x)
          ((and
            (number? x)
            (number? y)) (* x y))
          (else (list '* x y))))
(define (product? exp) (eq? '* (car exp)))
(define multiplier cadr)
(define (multiplicand expr)
  (fold-left
    make-product
    1
    (cddr expr)))

(define (make-exponentiation base power)
  (cond ((=number? power 0) 1)
        ((=number? power 1) base)
        (else (list '** base power))))
(define (exponentiation? exp) (eq? '** (car exp)))
(define base cadr)
(define exponent caddr)


(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) 
         '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) 
                                 set2)))
        (else (intersection-set (cdr set1) 
                                set2))))

(define (union-set set1 set2)
  (fold-right adjoin-set set1 set2))

(define (adjoin-ordered-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        ((> x (car set)) (cons (car set) (adjoin-ordered-set x (cdr set))))))

(define (union-ordered-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((= (car set1) (car set2))
          (cons (car set1) (union-ordered-set (cdr set1) (cdr set2))))
        ((< (car set1) (car set2)) 
          (cons (car set1) (union-ordered-set (cdr set1) set2)))
        (else
          (cons (car set2) (union-ordered-set set1 (cdr set2))))))


(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (tree->list tree)
  (if (null? tree)
      '()
      (append 
       (tree->list
        (left-branch tree))
       (cons (entry tree)
             (tree->list
              (right-branch tree))))))

(define (list->tree elements)

  (define (partial-tree elts n)
    (if (= n 0)
      (cons '() elts)
      (let ((left-size 
             (quotient (- n 1) 2)))
        (let ((left-result 
               (partial-tree 
                elts left-size)))
          (let ((left-tree 
                 (car left-result))
                (non-left-elts 
                 (cdr left-result))
                (right-size 
                 (- n (+ left-size 1))))
            (let ((this-entry 
                   (car non-left-elts))
                  (right-result 
                   (partial-tree 
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree 
                     (car right-result))
                    (remaining-elts 
                     (cdr right-result)))
                (cons (make-tree this-entry 
                                 left-tree 
                                 right-tree)
                      remaining-elts))))))))

  (car (partial-tree elements (length elements))))

(define
  tree
  (make-tree 7
             (make-tree 3
                        (make-tree 1 () ())
                        (make-tree 5 () ()))
             (make-tree 9
                        ()
                        (make-tree 11 () ()))))

(define (union-tree-set set1 set2)
  (list->tree 
    (union-ordered-set
      (tree->list set1)
      (tree->list set2))))

(union-tree-set
  tree
  (list->tree (list 2 4 5 6)))


(define (key x) x)
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((= given-key (key (entry set-of-records))) (entry set-of-records))
        ((< given-key (key (entry set-of-records))) (lookup given-key (left-branch set-of-records)))
        (else (lookup given-key (right-branch set-of-records)))))