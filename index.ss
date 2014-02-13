#lang racket

(provide extract-index
         the-web-index)

(define (drop n l)
  (if (= n 1) (cdr l)
      (drop (- n 1) (cdr l))))

(define (extract-index index)
  (define (helper l word acc i)
    (cond [(null? l) '()]
          [(char=? (car l) #\()
           (helper (cdddr l) word acc i)]
          [(char=? (car l) #\")
           (cond [(char=? (cadr l) #\space)
                  (helper (cdddr l) '()
                          (cons (list->string (reverse word))
                                acc) i)]
                 [(char=? (cadr l) #\))
                  (if (char=? (caddr l) #\space)
                      (helper (drop 5 l) '() '()
                              (cons (reverse (cons (list->string (reverse word))
                                                   acc))
                                    i))
                      (cons (reverse (cons (list->string (reverse word))
                                           acc))
                            i))])]
          [else (helper (cdr l)
                        (cons (car l) word)
                        acc i)]))
  (helper (string->list index) '() '() '()))

(define in (file->string (string-append (path->string (current-directory)) "/Pages/index.txt")))

(define the-web-index (extract-index in))
