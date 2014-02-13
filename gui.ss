#lang racket

;;;libraries
(require racket/gui)
(require net/sendurl)

;;;files
(require "project1.ss")
(require "spellcheck.ss")
(require "pagerank2.ss")

;;;main frame
(define browser 
  (new frame% 
       [label "Web Spider"] 
       [height 1080] 
       [width 1920] 
       [x 0] 
       [y 0]))

;;;font
(define serch-font (make-object font% 15 'roman 'italic 'normal #f 'smoothed #f 'aligned))

;;;search text field
(define serch
  (new text-field% 
       [label "Search  "] 
       [parent browser]
       ;[init-value "Search Here"]
       [style '(single)]
       [font serch-font]
       [vert-margin 50]
       [horiz-margin 20]
       [enabled #t]
       [callback (lambda (c e)
                   (cond ((equal? (send e get-event-type) 'text-field-enter)
                          (begin (set! page-count 0)
                                 (send results refresh)))
                         (else (send prompt refresh))))]))

(define page-count 0)

;;;bit maps
(define crawl (read-bitmap "crawl.png" 'png))
(define clear (read-bitmap "clear.png" 'png))
(define previou (read-bitmap "previous.png"))
(define nex (read-bitmap "next.png"))


;;;colors
(define c1 (make-object  color% "gainsboro"))
(define c2 (make-object color% "azure"))

;;;setting background color of textfield
(send serch set-field-background c1)

;;;editor of text-field
(define ed (send serch get-editor))

;;;horizontal pane
(define hp (new horizontal-pane% [parent browser]))

;;;vertical pane 
(define vp1 (new vertical-pane% [parent hp]))
(define vp2 (new vertical-pane% [parent hp]))

;;;crawl button 
(define craw (new button% 
                  [label crawl]
                  [parent vp1]
                  [callback (lambda (b e) (send results refresh))]))

;;;cancel button
(define cance (new button% 
                   [label clear]
                   [parent vp2]
                   [callback (lambda (b e) (begin (send ed erase)
                                                  (send prompt refresh)
                                                  (send results refresh)))]))
;;;clicking positions
(define clickx1 0)
(define clicky1 0)
(define clickx2 0)
(define clicky2 0)

;;;search results class
(define results1%
  (class canvas%
    (super-new)
    (define/override (on-event event) 
      (when (send event get-left-down) 
        (begin (set! clickx1 (send event get-x))
               (set! clicky1 (send event get-y))
               (openinbrowser clickx1 clicky1))))))

;;;page ranking
(define (func)
  (pagerank2 
   (append (map (lambda (x) (find-document x)) (bringwords (string-downcase (send serch get-value))))
           (map (lambda (x) (find-document x)) (bringwords (string-upcase (send serch get-value))))
           (map (lambda (x) (find-document x)) (bringwords (string-titlecase (send serch get-value)))))))

;;;search results in pages
(define (paged-name-list)
  (divide-to-pages (map (lambda (x) (vector-ref namevec (- (string->number x) 1)))
                        (func))))

(define (paged-link-list)
  (divide-to-pages (map (lambda (x) (vector-ref linkvec (- (string->number x) 1)))
                        (func))))

;;;result canvas
(define y-pos 0)

;;;search results canvas
(define results 
  (new results1%
       [parent vp1]
       [vert-margin 20]
       [horiz-margin 50]
       [paint-callback
        (lambda (canvas dc)
          (map (lambda (x) (begin (send dc draw-text x 10 y-pos) (set! y-pos (+ y-pos 30))))
               (list-ref (paged-name-list) page-count))
          (set! y-pos 0))]))

;;;link number 
(define (link-number y) (inexact->exact (floor (/ y 44.76470588235294))))

;;;open in browser function
(define (openinbrowser x y)
  (when (and (>= (- 860 x) 0)
             (>= (- 761 y) 0))
    (let ((lst (list-ref (paged-link-list) page-count)))
      (cond  ((< (link-number y) (length lst))
              (send-url (list-ref lst (link-number y))))))))

;;;prompt class
(define y-pos1 0)

(define prompt1
  (class canvas%
    (super-new)
    (define/override (on-event event)
      (when (send event get-left-down)
        (begin (set! clickx2 (send event get-x))
               (set! clicky2 (send event get-y))
               (copytext clickx2 clicky2))))))

(define sample-text "")

;;;serched text list
(define (text-list) (map add (moresearch (send serch get-value))))


;;;copytext function 
(define (copytext x y)
  (when (and (>= (- 860 x) 0)
             (>= (- 761 y) 0))
    (when (< (link-number y) (length (text-list)))
      (set! sample-text (list-ref (text-list) (link-number y)))
      (send ed erase)
      (send ed insert sample-text))))

;;;prompt canvas
(define prompt (new prompt1 
                    [parent vp2]
                    [vert-margin 20]
                    [horiz-margin 50]
                    [paint-callback 
                     (lambda (canvas dc)
                       (cond ((not (equal? (send serch get-value) ""))
                              (map (lambda (x) (begin (send dc draw-text x 10 y-pos1) (set! y-pos1 (+ y-pos1 30))))
                                   (text-list)))) (set! y-pos1 0))]))

;;;;;prompt canvas properties
(define dc (send prompt get-dc))
(send dc set-scale 1.5 1.5)
(send prompt set-canvas-background c2)
(send dc set-text-foreground "blue")

;;;;; results canvas properties
(define dc1 (send results get-dc))
(send dc1 set-scale 1.5 1.5)
(send results set-canvas-background c2)
(send dc1 set-text-foreground "blue")

;;;add words to a string
(define (add l)
  (cond ((null? l) "")
        (else (string-append (car l) " " (add (cdr l))))))

;;;previous button
(define previous
  (new button% 
       [label previou]
       [parent vp1]
       [callback (lambda (b e) (when (> page-count 0)
                                 (set! page-count (- page-count 1))
                                 (send results refresh)))]))
;;;next button
(define next
  (new button% 
       [label nex]
       [parent vp2]
       [callback (lambda (b e) (when (< page-count (max-page-count))
                                 (set! page-count (+ page-count 1))
                                 (send results refresh)))]))

;;;divide links to pages
(define (divide-to-pages nlis)
  (let ((lis (filter (lambda (x) (not (equal? x 0))) nlis)))
    (cond ((<= (length lis) 17) (list lis))
          (else (cons (take lis 17) (divide-to-pages (drop lis 17)))))))


(define (max-page-count) (- (length (paged-name-list)) 1))

(send browser show #t)