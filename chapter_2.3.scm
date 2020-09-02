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

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) 
                (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (choose-branch bit branch)
    (cond ((= bit 0) (left-branch branch))
          ((= bit 1) (right-branch branch))
          (else (error "bad bit: CHOOSE-BRANCH" bit)))) 

  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch 
                (car bits) 
                current-branch)))
          (if (leaf? next-branch)
              (cons 
               (symbol-leaf next-branch)
               (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) 
                        next-branch)))))

  (decode-1 bits tree))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) 
         (cons x set))
        (else 
         (cons (car set)
               (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set 
         (make-leaf (car pair)    ; symbol
                    (cadr pair))  ; frequency
         (make-leaf-set (cdr pairs))))))

(define sample-tree
  (make-code-tree 
   (make-leaf 'A 4)
   (make-code-tree
    (make-leaf 'B 2)
    (make-code-tree 
     (make-leaf 'D 1)
     (make-leaf 'C 1)))))

(define sample-message 
  '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define (encode message tree)
  (define (encode-symbol symbol tree)
    (cond ((null? tree) (error "no symbol in tree" symbol))
          ((leaf? tree)
            (if (eq? (symbol-leaf tree) symbol)
                ()
                (error "no symbol in tree" symbol)))
          ((contains? symbol (symbols (left-branch tree)))
            (cons 0 (encode-symbol symbol (left-branch tree))))
          (else 
            (cons 1 (encode-symbol symbol (right-branch tree))))))

  (if (null? message)
      ()
      (append 
       (encode-symbol (car message) tree)
       (encode (cdr message) tree))))

(define (contains? x list)
  (cond ((null? list) #f)
        ((eq? x (car list)) #t)
        (else (contains? x (cdr list)))))

(define (generate-huffman-tree pairs)
  (define (successive-merge nodes)
    (cond ((or (null? nodes)
               (null? (cdr nodes))) (car nodes))
          (else (successive-merge
                  (adjoin-set (make-code-tree (car nodes) (cadr nodes))
                              (cddr nodes))))))

  (successive-merge 
   (make-leaf-set pairs)))

(define rock-tree
  (generate-huffman-tree
    (list
      (list 'A 2)
      (list 'BOOM 1)
      (list 'GET 2)
      (list 'JOB 2)
      (list 'NA 16)
      (list 'SHA 3)
      (list 'YIP 9)
      (list 'WAH 1))))
(define rock-lyrics
  '(Get a job 
    Sha na na na na na na na na
    Get a job
    Sha na na na na na na na na
    Wah yip yip yip yip
    yip yip yip yip yip
    Sha boom))