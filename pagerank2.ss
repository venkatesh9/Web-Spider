#lang racket
(provide pagerank2)

(require racket/mpair)

(define (compact l)
  (foldr (lambda (x y)
           (append x y)) '() l))

(define (encode l)
  (foldr (lambda (x y)
           (if (massoc x y)
               (begin (set-mcdr! (massoc x y)
                                 (+ (mcdr (massoc x y)) 1))
                      y)
               (mcons (mcons x 1) y))) '() l))

(define (get a)
  (define a1 (mlist->list a))
  (define a2 (map (lambda (x) (cons (mcar x) (mcdr x))) a1))
  (define a3 (map (lambda (x) (cdr x)) a2))
  (define (h l acc or)
    (cond [(null? or) acc]
          [(eq? (car or) (cdar l)) (h (cdr l) (cons (caar l) acc) (cdr or))]
          [else (h (append (cdr l) (list (car l))) acc or)]))
  (h a2 '() (sort a3 <)))

(define (pagerank2 l)
  (get (encode (compact l))))
        
