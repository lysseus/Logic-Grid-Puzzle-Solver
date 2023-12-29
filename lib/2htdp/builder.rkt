#lang racket

;;;
;;; BUILDER
;;;

(provide run-builder)

(require 2htdp/image
         2htdp/universe
         utils/string
         utils/states
         utils/stack
         utils/cmd-queue
         utils/2htdp/image
         "pending-stream.rkt"
         (only-in "table.rkt" current-expected puzzle-complete?)
         "../solver-base.rkt"         
         "../common.rkt"
         "../lgps-states.rkt")

(define (key-handler ws ke)
  (debug-printf "key-handler state=~a ke=~a~%" (bld-world-state ws) ke)
  (state-handler ws STATES (bld-world-state ws) (key-normalize (bld-world-state ws) ke))
  ws)

(define KEY-FRAME-W (* 12 FONT-SIZE))
(define KEY-FRAME-H (* 2 FONT-SIZE))
(define STMT-FRAME-W (* 36 FONT-SIZE))
(define STMT-FRAME-H (* 2 FONT-SIZE))

(define MT (empty-scene MT-WIDTH MT-HEIGHT 'black))
(define MT-SEP (rectangle PIXELS MT-HEIGHT 'solid FONT-COLOR))

(define FRAME-PAD FONT-SIZE)
(define KEY-FRAME (rectangle KEY-FRAME-W KEY-FRAME-H 'solid 'transparent))
(define KEY-FRAME-INNER (rectangle (- KEY-FRAME-W FRAME-PAD) (- KEY-FRAME-H FRAME-PAD)
                                   'solid 'transparent))
(define REV-KEY-FRAME (rectangle KEY-FRAME-W KEY-FRAME-H 'solid HILITE-COLOR))
(define STMT-FRAME (rectangle STMT-FRAME-W STMT-FRAME-H 'solid 'transparent))
(define MESSAGE-FRAME (rectangle STMT-FRAME-W STMT-FRAME-H 'solid 'darkred))
(define REV-STMT-FRAME (rectangle STMT-FRAME-W STMT-FRAME-H 'solid HILITE-COLOR))

(define (draw-key-tmp ws)
  (color-frame/pixels FONT-COLOR 
                      (overlay/fit #:expand? #f
                                   #:w-pad PIXELS
                                   (text (bld-world-key-tmp ws) FONT-SIZE FONT-COLOR)                             
                                   KEY-FRAME)
                      PIXELS))

(define (draw-stmt-tmp-comps ws)
  (define stmt-txts
    (for/list ([v (bld-world-stmt-tmp ws)])
      (text (format "~a " v) FONT-SIZE FONT-COLOR)))
  (cond
    [(empty? stmt-txts) empty-image]
    [(= (length stmt-txts) 1) (car stmt-txts)]
    [else (apply beside stmt-txts)]))

(define (draw-exp-tmp-comps ws)
  (define exp-txts
    (for/list ([v (bld-world-exp-tmp ws)])
      (text (format "~a " v) FONT-SIZE FONT-COLOR)))
  (cond
    [(empty? exp-txts) empty-image]
    [(= (length exp-txts) 1) (car exp-txts)]
    [else (apply beside exp-txts)]))

(define (draw-stmt-tmp ws)
  (color-frame/pixels FONT-COLOR                      
                      (overlay/fit
                       #:expand? #f
                       #:w-pad PIXELS
                       (draw-stmt-tmp-comps ws)
                       STMT-FRAME)PIXELS))

(define (draw-exp-tmp ws)
  (color-frame/pixels FONT-COLOR                      
                      (overlay/fit
                       #:expand? #f
                       #:w-pad PIXELS
                       (draw-exp-tmp-comps ws)
                       STMT-FRAME)PIXELS))

(define (draw-num ws n sel#)
  (color-frame/pixels FONT-COLOR                      
                      (overlay/fit
                       #:expand? #f
                       #:w-pad PIXELS
                       (text (~a n) FONT-SIZE FONT-COLOR)
                       (square KEY-FRAME-H 'solid
                               (if (and (number? sel#) (= sel# n))
                                   HILITE-COLOR
                                   NO-COLOR)))
                      PIXELS))

(define (draw-key-val ws val)
  (color-frame/pixels FONT-COLOR
                      (overlay/fit
                       #:expand? #f
                       #:w-pad PIXELS
                       (text (string-trim (~a val) "%") FONT-SIZE FONT-COLOR)
                       (rectangle (- KEY-FRAME-W KEY-FRAME-H PIXELS) KEY-FRAME-H 'solid 'transparent))
                      PIXELS))

(define (draw-stmt-val ws val)
  (color-frame/pixels FONT-COLOR
                      (overlay/fit
                       #:expand? #f
                       #:w-pad PIXELS
                       (text (stmt->string val) FONT-SIZE FONT-COLOR)
                       (rectangle (- STMT-FRAME-W STMT-FRAME-H PIXELS)
                                  STMT-FRAME-H 'solid 'transparent))
                      PIXELS))

(define (draw-exp-val ws val)
  (color-frame/pixels FONT-COLOR
                      (overlay/fit
                       #:expand? #f
                       #:w-pad PIXELS
                       (text (~a val) FONT-SIZE FONT-COLOR)
                       (rectangle (- STMT-FRAME-W STMT-FRAME-H PIXELS)
                                  STMT-FRAME-H 'solid 'transparent))
                      PIXELS))

(define (draw-key-vals ws)
  (define id (State-id (bld-world-state ws)))
  (define task-id (State-task-id (bld-world-state ws)))
  (define txts
    (for/list ([val (hash-ref (bld-world-tbl ws) (bld-world-key ws))]
               [n (in-naturals 1)])
      (beside (draw-num ws n (bld-world-key# ws)) (draw-key-val ws val))))
  (define imgs
    (cond
      [(zero? (length txts)) empty-image]
      [(= 1 (length txts)) (car txts)]
      [else (apply above/align "left" txts)]))

  (cond
    [(and (eq? id 'cat) (eq? task-id 'input))
     (above/align "left"
                  imgs
                  (draw-key-tmp ws))]
    [else imgs]))

(define (draw-stmt-vals ws)
  (debug-printf "draw-stmt-vals ~a~%" (bld-world-state ws))
  (define id (State-id (bld-world-state ws)))
  (define task-id (State-task-id (bld-world-state ws)))
  (define stmts (hash-ref (bld-world-tbl ws) '%Statement))
  (define vals (take (drop stmts (bld-world-stmt-scr-top ws))
                     (min (- (length stmts) (bld-world-stmt-scr-top ws))
                          STMT-SCR-MAX)))
  (define txts
    (for/list ([val vals]
               [n (in-naturals (add1 (bld-world-stmt-scr-top ws)))])
      (beside (draw-num ws n (bld-world-stmt# ws)) (draw-stmt-val ws val))))
  (define imgs
    (cond
      [(zero? (length txts)) empty-image]
      [(= 1 (length txts)) (car txts)]
      [else (apply above/align "left" txts)]))

  (cond
    [(and (eq? id 'stmt) (eq? task-id 'edit))imgs]
    [else (above/align "left"
                       imgs
                       (draw-stmt-tmp ws))]))

(define (draw-exp-vals ws)
  (debug-printf "draw-exp-vals ~a~%" (bld-world-state ws))
  (define id (State-id (bld-world-state ws)))
  (define task-id (State-task-id (bld-world-state ws)))
  (define txts
    (for/list ([val (hash-ref (bld-world-tbl ws) '%Expected)]
               [n (in-naturals 1)])
      (beside (draw-num ws n (bld-world-exp# ws)) (draw-exp-val ws val))))
  (define imgs
    (cond
      [(zero? (length txts)) empty-image]
      [(= 1 (length txts)) (car txts)]
      [else (apply above/align "left" txts)]))

  (cond
    [(and (eq? id 'exp) (eq? task-id 'edit))imgs]
    [else (above/align "left"
                       imgs
                       (draw-exp-tmp ws))]))

(define (draw-key ws)
  (color-frame/pixels TITLE-COLOR
                      (overlay/fit #:expand? #f
                                   #:w-pad PIXELS
                                   (text (string-trim (~a (bld-world-key ws)) "%")
                                         FONT-SIZE TITLE-COLOR)
                                   (if (or (eq? (State-id (bld-world-state ws)) 'stmt)
                                           (eq? (State-id (bld-world-state ws)) 'exp))
                                       KEY-FRAME
                                       REV-KEY-FRAME))
                      PIXELS))

(define (draw-stmt ws)
  (color-frame/pixels TITLE-COLOR
                      (overlay/fit #:expand? #f
                                   #:w-pad PIXELS
                                   (text "Statement" FONT-SIZE TITLE-COLOR)
                                   (if (eq? (State-id (bld-world-state ws)) 'stmt)
                                       REV-STMT-FRAME
                                       STMT-FRAME))
                      PIXELS))

(define (draw-exp ws)
  (color-frame/pixels TITLE-COLOR
                      (overlay/fit #:expand? #f
                                   #:w-pad PIXELS
                                   (text "Expected" FONT-SIZE TITLE-COLOR)
                                   (if (eq? (State-id (bld-world-state ws)) 'exp)
                                       REV-STMT-FRAME
                                       STMT-FRAME))
                      PIXELS))

(define (draw-message ws)
  (cond
    [(false? (slv-world-message ws)) empty-image]
    [else (color-frame/pixels TITLE-COLOR
                              (overlay/fit #:expand? #f
                                           #:w-pad PIXELS
                                           (text (slv-world-message ws) FONT-SIZE TITLE-COLOR)
                                           MESSAGE-FRAME)
                              PIXELS)]))

(define (draw-key-menu ws)
  (above (draw-key ws)
         (draw-key-vals ws)))

(define (draw-stmt-menu ws)
  (above (draw-stmt ws)
         (draw-stmt-vals ws)))

(define (draw-exp-menu ws)
  (above (draw-exp ws)
         (draw-exp-vals ws)))

(define (draw-bld-world ws)
  (overlay (draw-message ws)
           (beside/align "top"                                
                         (draw-key-menu ws)
                         MT-SEP
                         (if (false? (bld-world-exp? ws))
                             (draw-stmt-menu ws)
                             (draw-exp-menu ws)))))

(define (draw-world ws)
  (debug-printf "ws=~a~%" ws)
  (if (eq? (State-id (bld-world-state ws)) 'solve)
      (draw-slv-world ws)
      (draw-bld-world ws)))

(define (render ws)
  (overlay/fit (draw-world ws) MT))

(define (new-world)
  (define tbl (make-hash '((%Category)
                           (%Statement)
                           (%Expected)
                           (%Main %Relationship %Category)                           
                           (%Relationship positive! negative! either-or! either-and! seq! next-to!))))
  (bld-world 0 #f
             (State 'main 'edit "") "" '%Main empty #f
             '() "" #f 0
             '() #f #f tbl))

(define (run-builder)
  (big-bang (new-world)
    (to-draw render)
    (on-key key-handler)
    (name "Logic Grid Puzzle Solver")))
