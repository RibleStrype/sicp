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

(define (split fst snd)
  (lambda (painter n)
    (if (= n 0)
      painter
      (let ((smaller ((split fst snd) painter (- n 1))))
        (fst painter
             (snd smaller smaller))))))

(define (make-vect x y)
  (list x y))
(define xcor-vect car)
(define ycor-vect cadr)

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1)
                (xcor-vect v2))
             (+ (ycor-vect v1)
                (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1)
                (xcor-vect v2))
             (- (ycor-vect v1)
                (ycor-vect v2))))

(define (scale-vect n v)
  (make-vect (* n (xcor-vect v))
             (* n (ycor-vect v))))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define origin-frame car)
(define edge1-frame cadr)
(define edge2-frame caddr)

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect 
      (scale-vect (xcor-vect v)
                  (edge1-frame frame))
      (scale-vect (ycor-vect v)
                  (edge2-frame frame))))))

(define (make-segment start end)
  (list start end))
(define start-segment car)
(define end-segment cadr)

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) 
         (start-segment segment))
        ((frame-coord-map frame) 
         (end-segment segment))))
     segment-list)))

(define outline-painter
        (segments->painter (list (make-segment (make-vect 0 0) (make-vect 0 1))
                                (make-segment (make-vect 0 1) (make-vect 1 1))
                                (make-segment (make-vect 1 1) (make-vect 1 0))
                                (make-segment (make-vect 1 0) (make-vect 0 0)))))

(define x-painter
        (segments->painter (list (make-segment (make-vect 0 0) (make-vect 1 1))
                               (make-segment (make-vect 1 0) (make-vect 0 1)))))

(define diamond-painter
        (segments->painter (list (make-segment (make-vect 0.5 0) (make-vect 0 0.5))
                               (make-segment (make-vect 0 0.5) (make-vect 0.5 1))
                               (make-segment (make-vect 0.5 1) (make-vect 1 0.5))
                               (make-segment (make-vect 1 0.5) (make-vect 0.5 0)))))


(define (transform-painter 
         painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame new-origin
                  (sub-vect (m corner1) 
                            new-origin)
                  (sub-vect (m corner2)
                            new-origin)))))))

(define (flip-vert painter)
  (transform-painter 
   painter
   (make-vect 0.0 1.0)   ; new origin
   (make-vect 1.0 1.0)   ; new end of edge1
   (make-vect 0.0 0.0))) ; new end of edge2

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (flip-horizon painter)
  (transform-painter
    painter
    (make-vect 1.0 1.0)
    (make-vect 1.0 0.0)
    (make-vect 0.0 1.1)))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left  (transform-painter 
                        painter1
                        (make-vect 0.0 0.0)
                        split-point
                        (make-vect 0.0 1.0)))
          (paint-right (transform-painter
                        painter2
                        split-point
                        (make-vect 1.0 0.0)
                        (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-top (transform-painter
                      painter1
                      split-point
                      (make-vect 0.0 1.0)
                      (make-vect 1.0 0.5)))
          (paint-bottom (transform-painter
                         painter2
                         (make-vect 0.0 0.0)
                         split-point
                         (make-vect 1.0 0.0))))
          (lambda (frame)
            (paint-top frame)
            (paint-bottom frame)))))


(define right-split (split beside below))
(define up-split (split below beside))