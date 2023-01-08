#lang racket

;;;
;;; CLAUSES
;;;

(provide (all-defined-out))

(require (for-syntax syntax/parse)
         "common.rkt"
         "queries.rkt")

;; Applies the proc to the category list
;; associated with name. If proc is nt a
;; procedure then it simply returns the value.
(define (category-apply name proc)
  (debug-printf "category-apply ~a ~a~%" name proc)
  (if (procedure? proc)
      (apply proc (? name))
      proc))


;;;
;;;; MACROS FOR CLAUSES
;;;;

(define-syntax (key-clause stx)
  (syntax-parse stx
    #:datum-literals (:)
    [(_ (category:expr : property:id))
     #'(cons (quote category)
             (category-apply (quote category) property))]
    [(_ (category:id : property:expr))
     #'(cons (quote category) (quote property))]))

(define-syntax (key-pair-clause stx)
  (syntax-parse stx
    [(_ (prop1:expr prop2:expr))
     #'(list (key-clause prop1) (key-clause prop2))]))

(define-syntax (rels-clause stx)
  (syntax-parse stx
    [(_ (prop:expr ...))
     #'(list (key-clause prop) ...)]))
