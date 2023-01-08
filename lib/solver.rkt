#lang racket

;;;
;;; SOLVER
;;;

(provide (all-from-out "solver-base.rkt")
         (all-from-out "2htdp/pending-stream.rkt")
         #%top)

(require syntax/parse/define
         "solver-base.rkt"
         "2htdp/pending-stream.rkt")

(define-simple-macro (#%top . x) 'x)
(define box 'box)
