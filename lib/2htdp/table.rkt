#lang racket

;;;
;;; TABLE
;;;

(provide build-table-images show-table current-answers)

(require (for-syntax syntax/parse)
         2htdp/image
         (only-in utils/2htdp/image images-max-dims)
         (only-in "../common.rkt" cat-text-images prop-text-images
                  FRAME-W-PAD FRAME-H-PAD FONT-SIZE FONT-COLOR
                  valid-key/c debug-printf)
         (only-in "../queries.rkt" ? ?= ?true))

(define current-answers (make-parameter empty))

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
  (current-table-images
   (for/hash ([v (append (hash->list cat-text-images)
                         (hash->list prop-text-images)
                         (list (cons '? (text (~a '?) FONT-SIZE FONT-COLOR))))])
     (values (car v)
             (overlay (cdr v)
                      (rectangle frame-w frame-h 'outline FONT-COLOR))))))

(define (make-table cs)
  (define c (last cs))
  (define prop-names (remove c cs))
  (define props (for/list ([p (? c)])
                  (cons c p)))
  (define rows (reverse (for/fold ([rows empty])
                     ([prop props])
             (define prop-val (cdr prop))
             (cons (cons prop-val (for/list ([prop-name (remove c cs)])
                                    (?true prop prop-name)))
                   rows))))
  (define header (apply beside
                        (map (λ (v) (hash-ref table-images v))
                       (cons c prop-names))))
  (define rows-img
    (apply above/align "left"
           (for/list ([row rows])
      (apply beside
             (map (λ (v) (hash-ref table-images (if (false? v) '? v)))
                         row)))))

  (define ans-img (validate (current-answers)))
  (define footer
    (overlay/align "center" "bottom"
                   ans-img
                   (rectangle (image-width header)
                              (+ FRAME-H-PAD (image-height ans-img))
                              'outline FONT-COLOR)))
  
  (above/align "left"
               header
               rows-img
               footer))

(define (show-table)
  (make-table (? 'cs)))

(define (validate ans)
  (->* () #:rest (listof (listof valid-key/c)) any)
  (debug-printf "~a~%" ans)
  (define sol (for/and ([row ans])
                (debug-printf "row=~a~%" row)
                (define 1st (first row))
                (define 2nd (second row))
                (define 3rd (third row))
                (define rel
                  (remove-duplicates (apply append
                                            '() (map set->list (?= 1st)))))
                (debug-printf "rel=~a~%" rel)
                (define ans (remove* rel row))
                (debug-printf "ans=~a~%" ans)
                (if (empty? ans) #t #f)))
  (debug-printf "sol=~a~%" sol)
  (text (format "~%Puzzle solution is ~a.~%"
          (cond
            [(empty? ans) 'unverified]
            [(false? sol) 'incorrect]
            [else 'correct]))
        (quotient FONT-SIZE 2) FONT-COLOR))
