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


(define (square-list items)
  (if (null? items)
      items
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list-2 items)
  (map square items))

(define (for-each f items)
    (cond ((null? items) #t)
          (else (f (car items)) (for-each f (cdr items)))))

(define (deep-reverse xs)
  (if (pair? xs)
    (append (deep-reverse (cdr xs))
            (list (deep-reverse (car xs))))
    xs))

(define (fringe xs)
  (cond ((null? xs) xs)
        ((not (pair? xs)) (list xs))
        (else (append (fringe (car xs))
                      (fringe (cdr xs))))))

(define (make-mobile left right)
  (list left right))
(define left-branch car)
(define (right-branch mobile)
  (car (cdr mobile)))

(define (make-branch length structure)
  (list length structure))
(define branch-length car)
(define (branch-structure branch)
  (car (cdr branch)))

(define (branch-weight branch)
    (let ((struct (branch-structure branch)))
         (if (pair? struct)
           (total-weight struct)
           struct)))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (balanced? mobile)
  (define (structure-balanced? struct)
    (if (pair? struct)
      (balanced? struct)
      #t))

  (and (structure-balanced? (branch-structure (left-branch mobile)))
       (structure-balanced? (branch-structure (right-branch mobile)))
       (= (* (branch-length (left-branch mobile)) (branch-weight (left-branch mobile)))
          (* (branch-length (right-branch mobile)) (branch-weight (right-branch mobile))))))

(define x (make-mobile (make-branch 3 5)
                       (make-branch 4 
                                    (make-mobile (make-branch 6 7)
                                                 (make-branch 1 8)))))

(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (tree-map proc sub-tree)
           (proc sub-tree)))
       tree))

(define (square-tree tree)
  (tree-map square tree))

(define (subsets s)
  (if (null? s)
      (list s)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (ss)
                            (cons (car s) ss))
                          rest)))))

(define (accumulate combine initial list)
  (if (null? list)
    initial
    (combine (car list)
             (accumulate combine initial (cdr list)))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) 
              () sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(define 
  (horner-eval x coefficient-sequence)
  (accumulate 
   (lambda (this-coeff higher-terms)
     (+ this-coeff (* x higher-terms)))
   0
   coefficient-sequence))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      ()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (accumulate-n * 1 (list v w))))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product v row)) m))

(define (transpose mat)
  (accumulate-n cons () mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

(define fold-right accumulate)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (reverse-fold-right sequence)
  (fold-right 
   (lambda (x y) (append y (list x))) () sequence))

(define (reverse-fold-left sequence)
  (fold-left 
   (lambda (x y) (cons y x)) () sequence))

(define (enumerate from to)
  (if (> from to)
    ()
    (cons from (enumerate (+ from 1) to))))

(define (flatmap proc seq)
  (fold-right append () (map proc seq)))

(define (unique-pairs n)
  (flatmap (lambda (i)
            (map (lambda (j) (list j i))
                 (enumerate 1 (- i 1))))
           (enumerate 1 n)))


(define (unique-triples n)
  (flatmap (lambda (i)
            (flatmap (lambda (j) 
                      (map (lambda (k) (list k j i)) 
                           (enumerate 1 (- j 1))))
                     (enumerate 1 (- i 1))))
           (enumerate 1 n)))

(define (triple-sum n s)
  (filter (lambda (t) (= (+ (car t)
                            (cadr t)
                            (caddr t))
                         s))
          (unique-triples n)))

(define (forall p seq)
  (fold-left (lambda (x y) (and x y)) #t (map p seq)))

(define (queens board-size)
  (define empty-board ())
  
  (define (adjoin-position row col positions)
    (cons (list row col) positions))

  (define (on-diagonal? row col p)
    (= (abs (- (car p) row))
       (abs (- (cadr p) col))))

  (define (pos-safe? row col)
    (lambda (p)
      (cond ((= col (cadr p)) #t)
            ((= row (car p)) #f)
            (else (not (on-diagonal? row col p))))))

  (define (safe? col positions)
    (let ((row (caar (filter (lambda (p) (= col (cadr p)))
                             positions))))
      (forall (pos-safe? row col) positions)))

  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) 
           (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position 
                    new-row 
                    k 
                    rest-of-queens))
                 (enumerate
                  1 
                  board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))