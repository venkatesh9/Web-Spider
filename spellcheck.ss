#lang racket
(provide correct moresearch bringwords commonwords)

(define words (file->lines "dictionary.txt"))
(define words-vector (build-vector 26 (lambda (x) '())))

(define (correct s)
  (let ((lst (string->list s)))
    (define (helper lst)
      (cond ((null? lst) lst)
            ((equal? (car lst) #\space) (helper (cdr lst)))
            (else (cons (car lst) (helper (cdr lst))))))
    (list->string (helper lst))))

(define (index s) (- (char->integer (string-ref (string-downcase s) 0)) 97))

(define (short-list word-list)
  (let ((num (length word-list)))
    (define (helper n lis)
      (cond ((= n num)  'done)
            (else (begin (vector-set! words-vector (index (car lis)) (cons (car lis) (vector-ref words-vector (index (car lis)))))
                         (helper (+ n 1) (cdr lis))))))
    (helper 0 word-list)))

(short-list (reverse words))

(define (word-search strng)
  (define (helper lis acc)
    (cond ((or (= (length acc) 10) (null? lis)) (reverse acc))
          ((< (string-length (car lis)) (string-length strng)) (helper (cdr lis) acc))
          ((equal? (take (string->list (car lis)) (string-length strng)) (string->list (string-downcase strng)))
           (helper (cdr lis) (cons (car lis) acc)))
          (else (helper (cdr lis) acc))))
  (let ((nlst (member strng (vector-ref words-vector (index strng)))))
    (helper (if nlst nlst
                (vector-ref words-vector (index strng))) '())))

(define (simplify strn)
  (let* ((modi (correct (string-downcase strn)))
         (wlis (vector-ref words-vector (index modi))))
    (define (helper lis)
      (cond ((member lis wlis) (word-search lis))
            (else (helper (substring lis 0 (- (string-length lis) 1))))))
    (helper modi)))

(define (bringwords s)
  (filter (lambda (x) (not (or (= (string-length x) 0) (= (string-length x) 1)))) (regexp-split #rx"[^a-zA-Z]" s)))

(define (cprod l)
  (define (ins l1 l2)
    (if (null? l1) '()
        (append (map (lambda (x) (cons (car l1) x)) l2) (ins (cdr l1) l2))))
  (if (null? (cdr l)) (map (lambda (x) (list x)) (car l))
      (ins (car l) (cprod (cdr l)))))

(define (moresearch strng)
  (if (or (= (string-length strng) 1) (equal? strng "")) '()
      (cprod (map (lambda (x) (if (and (>= (length (bringwords strng)) 4) (>= (length (simplify x)) 2))
                                  (take (simplify x) 2) (simplify x))) (bringwords strng)))))

(define commonwords (file->lines "commonwords.txt"))