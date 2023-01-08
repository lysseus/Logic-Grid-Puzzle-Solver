#lang racket

;;;
;;; INIT
;;;

(provide init)

(require (for-syntax syntax/parse)
         (only-in "common.rkt" category+properties/c current-puzzle)
         (only-in "2htdp/grid.rkt" build-grid-images)
         (only-in "2htdp/table.rkt" build-table-images))

(define-syntax (init stx)
  (syntax-parse stx
    [(_ c+ps ...) #'(:init (quote c+ps) ...)]))
;; Creates a puzzle-hash from the cs list of category subliss.
;; The first element of a category sublist is the category name, followed by
;; category elements. 
(define/contract (:init . inits)
  (->* () #:rest category+properties/c any)
  (define matrix (cons (length inits)
                       (for/fold ([cols #f])
                                 ([c+ps inits])
                         (define ps (rest c+ps))
                         (cond
                           [(false? cols) (length ps)]
                           [(= (length ps) cols) cols]
                           [else (error
                                  (format "init category ~a not the expected length ~a of initial category."
                                          (car c+ps) cols))]))))
  (define ls (for/fold ([ls empty])
                       ([c+ps inits])
               (cons
                (for/list ([ps (rest c+ps)])
                  (cons (first c+ps) ps))
                ls)))
  (define order (for/fold ([acc empty])
                          ([x (range (length ls))])
                  (append acc
                          (reverse (for/fold ([acc empty])
                                             ([y (range (length ls))])
                                     (if (< x y)
                                         (cons (list x y) acc)
                                         acc))))))
  (define cs-maxima
    (for/list ([c+ps inits]) (apply max (map (compose string-length ~a) c+ps))))
  (define grid (make-hash
                (append (list (cons 'inits inits))
                        (list (cons 'cs (map car inits)))
                        (list (cons 'cs-maxima cs-maxima))
                        (list (cons 'matrix matrix))
                        (list (cons 'cs-order order))
                        inits
                        (map (λ (v) (cons v '?))
                             (for/fold ([acc empty])
                                       ([o order])
                               (append acc
                                       (for*/list ([x (list-ref ls (first o))]
                                                   [y (list-ref ls
                                                                (second o))])
                                         (set x y))))))))
  (current-puzzle grid)
  (build-grid-images inits)
  (build-table-images inits))