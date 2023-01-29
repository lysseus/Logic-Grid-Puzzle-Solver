#lang racket

;;;
;;; GRID
;;;

(provide build-grid-images show-grid)

(require (for-syntax syntax/parse)
         2htdp/image
         utils/2htdp/image
         "../common.rkt"
         "../queries.rkt")

(define CAT-FRAME-PIXELS 1)

(define-syntax grid-images
  (λ (stx)
    (syntax-parse stx [grid-images:id #'(current-grid-images)])))
(define current-grid-images (make-parameter #f))

;; Create a table of images
(define (build-grid-images inits)
  ;; Convert properties to text images.
  (current-prop-text-images
   (for/hash ([V (flatten (map cdr inits))])
     (values V (text (~a V) FONT-SIZE FONT-COLOR))))
  
  (define-values (prop-text-w prop-text-h) (images-max-dims prop-text-images))
    
  ;; Convert categories to text images
  (current-cat-text-images
   (for/hash ([v (flatten (map car inits))])
     (values v (text (~a v) FONT-SIZE FONT-COLOR))))

  (define-values (cat-text-w cat-text-h)
    (images-max-dims cat-text-images))

  ;; Frame the text images.
  (define (build-images #:v-align (v-align 'left)
                        text-w frame-w text-h frame-h pairs) 
    (map (λ (p)
           (cons (car p)
                 (overlay
                  (overlay/align v-align 'center
                                 (cdr p)
                                 (rectangle text-w text-h 'outline 'transparent))
                  (rectangle frame-w frame-h 'outline PROP-FRAME-COLOR))))
         pairs))

  ;; Number of properties.
  (define prop# (length (rest (first inits))))

  ;; Maximum category frame width and prop frame height.
  (define-values (cat-frame-w prop-frame-h)
    (cond
      [(> (+ FRAME-W-PAD cat-text-w)
          (* prop# (+ FRAME-H-PAD prop-text-h)))
       (define w (+ FRAME-W-PAD cat-text-w))
       (values w (quotient w prop#))]
      [else
       (define w (* prop# (+ FRAME-H-PAD prop-text-h)))
       (values w (+ FRAME-H-PAD prop-text-h))]))

  ;; Maximum category frame height.
  (define cat-frame-h (+ cat-text-h FRAME-H-PAD))

  ;; Maximum prop frame width.
  (define prop-frame-w (+ prop-text-w FRAME-W-PAD))

  ;; Build category images.
  (define cat-imgs (build-images #:v-align 'center
                                 cat-text-w cat-frame-w cat-text-h cat-frame-h
                                 (hash->list cat-text-images)))

  ;; Build prop images.
  (define prop-imgs (build-images prop-text-w prop-frame-w prop-text-h prop-frame-h
                                  (hash->list prop-text-images)))

  (define (make-box-image v c)
    (define img (text (~a v) FONT-SIZE c))
    (define f (min (/ (image-width img) prop-frame-h)
                   (/ (image-height img) prop-frame-h)))
    (overlay
     (scale f img)
     (square prop-frame-h 'outline FRAME-COLOR)))

  (define box-imgs
    (for/list ([k (list BOX-TRUE BOX-FALSE BOX-UNKNOWN)]
               [c (list TRUE-COLOR FALSE-COLOR FONT-COLOR)])
      (cons k (make-box-image k c))))  
  
  (current-grid-images (make-hash (append cat-imgs prop-imgs box-imgs))) )

;; Make the  header image for a category.
(define (make-header #:orient (orient 'side) cat)
  (define cvs (cons cat (? cat)))
  (define cat-img (hash-ref grid-images (car cvs)))
  (define props-img (apply above empty-image
                           (for/list ([c (rest cvs)])
                                   (hash-ref grid-images c))))
  (define header
    (if (eq? orient 'side)
        (beside (rotate 90 cat-img)
                props-img)
        (above cat-img
               (rotate 90 props-img))))
  (color-frame/pixels SUB-GRID-COLOR 
   (overlay (rectangle (image-width header)
                      (image-height header) 'outline SUB-GRID-COLOR)
           header)
   CAT-FRAME-PIXELS))

;; Make a sub-grid for the intersection of cattegory properties.
(define (make-sub-grid cat-name1 cat-name2)
  (define boxes
    (for/list ([row (?xcat cat-name1 cat-name2)])
      (map (λ (v) (cond
                    [(false? v) BOX-FALSE]
                    [(true? v) BOX-TRUE]
                    [else BOX-UNKNOWN]))
           row)))
  (define box-images
    (for/list ([row boxes])
      (apply beside
             empty-image
             (map (λ (v) (hash-ref grid-images v))
                  row))))
  (define sub-grid (apply above empty-image box-images))
  (color-frame/pixels SUB-GRID-COLOR 
   (overlay (square (image-width sub-grid) 'outline SUB-GRID-COLOR)
           sub-grid)
   CAT-FRAME-PIXELS))

(define (make-grid inits (blank-txt #f))
  (define cs (? 'cs))
  (define tops (take cs (sub1 (length cs))))
  (define sides (reverse (rest cs)))

  ;; Produces a list of columns 
  (define rows
    (for/list ([n (reverse (range 1 (add1 (length tops))))])
      (take tops n)))

  (define boxes-img
    (apply above/align "left"
           empty-image
           (for/list ([row rows]
                      [side sides])
     
             (define imgs
               (for/list ([col row])
                 (make-sub-grid side col)))
             (define img (if (= (length imgs) 1)
                             (car imgs)
                             (apply beside imgs)))
             img)))
  (define side-img
    (apply above
           empty-image
           (for/list ([side sides])
             (make-header side))))
  (define top-img
    (apply beside
           empty-image
           (for/list ([top tops])
             (make-header top #:orient 'top))))
  (define outer-sqr (square (image-width side-img) 'outline FRAME-COLOR))
  (define inner-sqr (square (* 5 (quotient (image-width side-img) 10)) 'solid 'transparent))
  (define blank
    (cond
      [(false? blank-txt) sqr]
      [else
       (overlay (place-image/fit (text blank-txt FONT-SIZE FONT-COLOR) inner-sqr)
                outer-sqr)]))
  
  (above (beside blank top-img)
         (beside side-img boxes-img)))

(define (show-grid (blank-txt #f))
  (make-grid (? 'inits) blank-txt))
