#lang racket

;;;
;;; GRID
;;;

(provide print-grid λprint-grid slen!)

(require (for-syntax syntax/parse))

(define (pad v)
  (substring
   (string-append (~a v) (make-string slen #\space)) 0 slen))

(define-syntax slen
  (λ (stx)
    (syntax-parse stx [slen:id #'(current-slen)])))
(define-syntax slen!
  (λ (stx)
    (syntax-parse stx      
      [(_ v:number)
       #'(current-slen v)])))
(define current-slen (make-parameter 3))

(define (print-row #:name0 (name0 #\space) names)
  (printf "~%~a" (pad name0))
  (for ([t names])
    (printf "|")
    (for ([v t])
      (define s (pad v))
      (printf "|~a" s)))
  (printf "||"))

(define (print-cols cols)
  (for ([col cols]
        [n (reverse (range 1 (add1 (length cols))))])    
    (for ([name col])
      (print-row #:name0 name (make-list n
                                         (make-list (length col) #\space))))
    (dashes n (length col))))

(define (dashes cats# props# (val #\-))
  (printf "~%~a"
          (make-string (+ 2 cats# slen (* cats# props# (add1 slen))) val)))
(define-syntax (print-grid stx)
  (syntax-parse stx
    [(_ c ...) #'(λprint-grid (quote c) ...)]))
(define (λprint-grid . cs)
  (define top (map rest (take cs (sub1 (length cs)))))
  (define side (map rest (reverse (drop cs 1))))
  (define cats# (length top))
  (define props# (length (car side)))
  (dashes cats# props#)
  (print-row top)
  (dashes cats# props#)
  
  (print-cols side))
