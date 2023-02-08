#lang racket

;;;
;;; COMMON
;;;

(provide (all-defined-out))

(require (for-syntax syntax/parse))


;;;
;;; DISPLAY CONSTANTS
;;;
(define UNKNOWN '?)
(define TRUE  #\u2714)
(define FALSE #\u2718)

(define BOX-UNKNOWN #\space)
(define BOX-TRUE  #\u2714)
(define BOX-FALSE #\u2718)

(define FONT-COLOR 'white)
(define REV-FONT-COLR 'black)
(define TITLE-COLOR 'white)
(define FALSE-COLOR 'red)
(define TRUE-COLOR 'green)
(define FRAME-COLOR 'lightgray)
(define PROP-FRAME-COLOR 'transparent)
(define SUB-GRID-COLOR 'white)
(define NO-COLOR 'transparent)
(define HILITE-COLOR 'purple)

(define FONT-SIZE 24)

(define MSG-FRAME-W (* 36 FONT-SIZE))
(define MSG-FRAME-H (* 2 FONT-SIZE))

(define FRAME-W-PAD 8)
(define FRAME-H-PAD 8)
(define PIXELS 4)

(define-syntax puzzle
  (λ (stx)
    (syntax-parse stx [puzzle:id #'(current-puzzle)])))
(define current-puzzle (make-parameter #f))

(define-syntax cat-text-images
  (λ (stx)
    (syntax-parse stx [cat-text-images:id #'(current-cat-text-images)])))
(define current-cat-text-images (make-parameter #f))

(define-syntax prop-text-images
  (λ (stx)
    (syntax-parse stx [prop-text-images:id #'(current-prop-text-images)])))
(define current-prop-text-images (make-parameter #f))

;;;
;;; PREDICATES
;;;

(define (1? v) (= v 1))
(define (true? v) (eq? v #t))
(define (unknown? v) (eq? v'?))


;;;
;;; CONTRACTS FOR PUZZLE DEFINITION
;;;

(define category/c symbol?)
(define property/c (or/c symbol? number?))
(define category+properties/c (listof (cons/c category/c (listof property/c))))
(define key/c (cons/c category/c property/c))


;;;
;;; CONTRACTS FOR PUZZLE STATEMENTS
;;;

(define valid-category/c (and/c category/c (λ (v)
                                               (hash-has-key? puzzle v))))
(define valid-key/c
  (and/c key/c (λ (v)
                  (and (valid-category/c (car v))
                       (member (cdr v) (hash-ref puzzle (car v)))))))

;;;
;;; HELPERS
;;;


;; Suppresses a user error that may be thrown by a statement.
;; Used for testing/debugging.
(define-syntax (suppress-error stx)
  (syntax-parse stx
    [(_ s1)
     #'(with-handlers ([exn:fail:user?
                        (λ (e) (debug-printf "~a~%" (exn-message e)) #f)])
         s1
         #t)]))


;;;
;;;  DEBUG
;;;

(define debug (make-parameter #f))

(define (debug-printf . vs)
  (cond
    [(false? (debug))
     (void)]
    [(true? (debug))
     (apply printf vs)]))

(define current-log (make-parameter #f))
