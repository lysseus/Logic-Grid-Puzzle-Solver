#lang racket

;;;
;;; TABLE
;;;

(provide build-table-images show-table current-expected puzzle-complete?)

(require (for-syntax syntax/parse)
         2htdp/image
         (only-in utils/2htdp/image images-max-dims overlay/fit)
         (only-in "../common.rkt" cat-text-images prop-text-images
                  FRAME-W-PAD FRAME-H-PAD FONT-SIZE FONT-COLOR
                  UNKNOWN TRUE FALSE TRUE-COLOR FALSE-COLOR PIXELS
                  valid-key/c debug-printf)
         (only-in "../queries.rkt" ? ?= ?true))

(define current-expected (make-parameter empty))

(define-syntax table-images
  (λ (stx)
    (syntax-parse stx [table-images:id #'(current-table-images)])))
(define current-table-images (make-parameter #f))


;; Create a hash of images for the table.
(define (build-table-images inits)
  ;; Get the categories.
  (define cs (map car inits))    
  (define-values (cat-text-w cat-text-h) (images-max-dims cat-text-images))
  (define-values (prop-text-w prop-text-h) (images-max-dims prop-text-images))
  (define max-w (max cat-text-w prop-text-w))
  (define max-h (max cat-text-h prop-text-h))
  (define frame-w (+ max-w FRAME-W-PAD))
  (define frame-h (+ max-h FRAME-H-PAD))
  (define inner (rectangle (- frame-w (* 2 PIXELS))
                           (- frame-h (* 2 PIXELS)) 'solid 'transparent))
  (define outer (rectangle frame-w frame-h 'outline FONT-COLOR))
  (current-table-images
   (for/hash ([v (append (hash->list cat-text-images)
                         (hash->list prop-text-images)
                         (list (cons 'expected? (text (~a 'Expected?) FONT-SIZE FONT-COLOR)))
                         (list (cons '? (text (~a UNKNOWN) FONT-SIZE FONT-COLOR)))
                         (list (cons #f (text (~a FALSE) FONT-SIZE FALSE-COLOR)))
                         (list (cons #t (text (~a TRUE) FONT-SIZE TRUE-COLOR))))])
     (values (car v)
             (overlay (overlay/fit (cdr v)
                                   inner)
                      outer)))))

(define (draw-vld-box row cats exps)
  (define result (validate row cats exps))
  (hash-ref table-images result))

(define (make-table cats)
  (debug-printf "cs=~a~%" cats)
  (define last-cat (last cats))
  (debug-printf "c=~a~%" last-cat)
  (define top-cats (remove last-cat cats))
  (debug-printf "top-cats=~a~%" top-cats)
  (define keys (for/list ([p (? last-cat)])
                 (cons last-cat p)))
  (debug-printf "keys=~a~%" keys)
  (define rows (reverse (for/fold ([rows empty])
                                  ([key keys])
                          (define prop (cdr key))
                          (cons (cons prop (for/list ([cat (remove last-cat cats)])
                                             (?true key cat)))
                                rows))))
  (debug-printf "rows=~a~%" rows)
  (define header (beside (apply beside
                                (map (λ (v) (hash-ref table-images v))
                                     (cons last-cat top-cats)))
                         (hash-ref table-images 'expected?)))
  (define rows-img
    (apply above/align "left"
           empty-image
           (for/list ([row rows])
             (beside (apply beside
                            (map (λ (v) (hash-ref table-images (if (false? v) '? v)))
                                 row))
                     (draw-vld-box row cats (current-expected))))))    
  
  (above/align "left"
               header
               rows-img))

(define (show-table)
  (make-table (? 'cs)))

(define (member/car v lst)
  (define vs (list->vector (map car lst)))
  (define n (vector-member v vs))
  (if (false? n)
      #f
      (drop lst n)))

(define (validate row cats ans)
  (define (f key cat-order ans)
    (define vs
      (for/first ([ans ans]
                  #:when (member key ans))
        ans))
    (define (loop cat-order (acc empty))
      (cond
        [(empty? cat-order) (reverse acc)]
        [else
         (define cat (car cat-order))
         (define v (cdar (member/car cat vs)))
         (loop (cdr cat-order) (cons v acc))]))
  
    (loop cat-order))
  (cond [(empty? ans) '?]
        [(member #f row) '?]
        [else         
         (define order (cons (last cats) (take cats (sub1 (length cats)))))
         (equal? row (f (cons (car order) (car row)) order ans))]))

(define (puzzle-complete? ws cats)
  (define last-cat (last cats))
  (define top-cats (remove last-cat cats))
  (define keys (for/list ([p (? last-cat)])
                 (cons last-cat p)))
  (define rows (reverse (for/fold ([rows empty])
                                  ([key keys])
                          (define prop (cdr key))
                          (cons (cons prop (for/list ([cat (remove last-cat cats)])
                                             (?true key cat)))
                                rows))))
  (for/and ([row rows])
    (andmap (compose not false?) row)))
