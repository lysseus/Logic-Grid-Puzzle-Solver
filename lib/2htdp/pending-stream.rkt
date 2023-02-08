#lang racket

;;;
;;; PENDING-STREAM
;;;

(provide (struct-out slv-world) go! λgo! draw-slv-world)

(require (for-syntax syntax/parse)
         2htdp/image
         2htdp/universe
         utils/stack
         utils/cmd-queue
         utils/2htdp/text
         utils/2htdp/image
         (only-in "../common.rkt" TITLE-COLOR MSG-FRAME-H MSG-FRAME-W PIXELS)
         "../clauses.rkt"
         "grid.rkt"
         "table.rkt")

(struct slv-world (count message)
  #:mutable #:transparent)

(define (key-handler ws ke)
  (cond
    [(key=? ke " ")
     (stream-pending)
     (set-slv-world-count! ws (add1 (slv-world-count ws)))])
  ws)

;; Wrap the image in a rectangular transparent padding
;; of pad pixels on each side.
(define (pad-image img pad)
  (overlay img
           (rectangle (+ (* 2 pad) (image-width img))
                      (+ (* 2 pad) (image-height img))
                      'solid 'transparent)))

(define FONT-COLOR 'white)
(define CURRENT-COLOR 'darkblue)
(define PENDING-COLOR 'black)
(define SUCCESSFUL-COLOR 'darkgreen)
(define FAILED-COLOR 'darkred)
(define EXPIRED-COLOR  'darkred)

(define MT-WIDTH  1200) ; Maximum width of screen 1268. Rounding down
(define MT-HEIGHT 650)  ; Maximum height of screen 655. Rounding down.
(define MT (empty-scene MT-WIDTH MT-HEIGHT 'black))
(define MT-PAD 20)

(define MT-GRID-WIDTH (quotient MT-WIDTH 2))
(define MT-GRID-HEIGHT MT-HEIGHT)
(define MT-GRID (empty-scene MT-GRID-WIDTH MT-GRID-HEIGHT 'black))

(define MT-PROCESS-WIDTH (quotient MT-WIDTH 2))
(define MT-PROCESS (empty-scene MT-PROCESS-WIDTH MT-HEIGHT 'black))
(define DIVIDER (rectangle 5 MT-HEIGHT 'solid FONT-COLOR))

(define FONT-SIZE 14)

(define FRAME-H (* 2 FONT-SIZE))
(define FRAME-W (- MT-PROCESS-WIDTH (* 2 FRAME-H)))
(define FRAME (rectangle FRAME-W FRAME-H 'outline 'white))
(define STMT-FRAME-W (- FRAME-W 10))
(define STMT-FRAME (rectangle STMT-FRAME-W FRAME-H 'solid 'transparent))
(define MSG-FRAME (rectangle MSG-FRAME-W MSG-FRAME-H 'solid 'blue))

(define S#-IMG (overlay (text "STMT#" (quotient FONT-SIZE 3) FONT-COLOR)
                        (square FRAME-H 'outline FONT-COLOR)))
(define REM-IMG (overlay (text "REM" (quotient FONT-SIZE 3) FONT-COLOR)
                        (square FRAME-H 'outline FONT-COLOR)))
(define PENDING-HDR (beside
                     S#-IMG
                     (overlay (text "PENDING" FONT-SIZE FONT-COLOR)
                             FRAME)
                     REM-IMG))
(define FAILED-HDR (beside
                    S#-IMG
                    (overlay (text "FAILED" FONT-SIZE FONT-COLOR)
                             FRAME)
                    REM-IMG))
(define SUCCESSFUL-HDR (beside
                        S#-IMG
                        (overlay (text "SUCCESSFUL" FONT-SIZE FONT-COLOR)
                                 FRAME)
                        REM-IMG))

(define SPACE (square FONT-SIZE 'solid 'transparent))

(define (~p v)
  (cond
    [(procedure? v)
     (string-trim
      (string-trim (string-trim (~a v) "#<procedure:" #:left? #t)
                   "λ" #:left? #t)
      ">" #:right? #t)]
    [else (~a v)]))

(define (~pa v) (string-replace (~a v) " . " " : "))

(define (draw-cmd c clr)
  (define stmt# (cmd-stmt# c))
  (define tries (cmd-tries c))
  (define proc (cmd-proc c))
  (define args (cmd-args c))
  (define stmt#-img
    (overlay
     (text (~a stmt#) FONT-SIZE FONT-COLOR)
     (square (* 2 FONT-SIZE) 'outline FONT-COLOR)))
  
  (define-values (stmt-img stmt-dims)
    (text-tok-wrap
                (format "~a ~a" (~p proc) (~pa args))
                FONT-SIZE FONT-COLOR STMT-FRAME-W))
  
  (define tries-img
    (overlay
     (text (~a tries) FONT-SIZE FONT-COLOR)
     (square (* 2 FONT-SIZE) 'outline FONT-COLOR)))

  (define status-frame (rectangle MT-PROCESS-WIDTH FRAME-H 'solid clr))
  (overlay (beside stmt#-img
          (overlay (overlay/align "left" "center"
                                  stmt-img
                                  STMT-FRAME)
                   FRAME)
          tries-img)
           status-frame))

(define (draw-queue queue queue-hdr)
  (define imgs
    (for/list ([c (Lifo-data queue)]
               [n (in-naturals)])
      (define tries (cmd-tries c))
      (draw-cmd c (cond
                    [(equal? queue successful) SUCCESSFUL-COLOR]
                    [(and (number? tries) (zero? tries)) EXPIRED-COLOR]
                    [(equal? queue failed) FAILED-COLOR]
                    [(zero? n) CURRENT-COLOR]
                    [else PENDING-COLOR]))))
  (if (empty? imgs)
      queue-hdr
      (apply above/align "left"
             queue-hdr imgs)))

(define (draw-processing-window ws)  
  (define img
    (above/align "left"
                 (draw-queue pending PENDING-HDR)
                 SPACE
                 (draw-queue failed FAILED-HDR)
                 SPACE
                 (draw-queue successful SUCCESSFUL-HDR)))
  (pad-image img MT-PAD))

(define (draw-grid ws)
  (define step#  (if (zero? (slv-world-count ws)) "" (slv-world-count ws)))
  (define stmt# (if (false? processing-cmd) "" (cmd-stmt# processing-cmd)))
  (define blank-txt (format "STEPS~%   ~a~%~%STMT#~%   ~a"
                            step#
                            stmt#))
  (define grid (show-grid blank-txt))
  (define grid-w (image-width grid))  
  (pad-image grid MT-PAD))

(define (draw-table ws)
  (define table (show-table))
  (pad-image table MT-PAD))

(define (draw-slv-world ws)
  (overlay(draw-message ws)
          (beside/align "top"                
                        (draw-grid ws)
                        DIVIDER
                        (above
                         (draw-processing-window ws)
                         (draw-table ws)))))

(define (draw-message ws)
  (cond
    [(false? (slv-world-message ws)) empty-image]
    [else (color-frame/pixels TITLE-COLOR
                              (overlay/fit #:expand? #f
                                           #:w-pad PIXELS
                                           (text (slv-world-message ws) (* 2 FONT-SIZE) TITLE-COLOR)
                                           MSG-FRAME)
                              (* 2 PIXELS))]))

(define (render ws)
  (place-image/fit (draw-slv-world ws) MT))

(define (new-slv-world ans)
  (slv-world 0))

(define (done? ws) (and (zero? (size-active pending)) (zero? (size-active failed))))

(define-syntax (go! stx)  
  (syntax-parse stx
    [(_)
     #'(λgo!)]
    [(_ ans ...)
     #'(λgo! (rels-clause ans) ...)]))
(define (λgo! . ans)
  (initialize-cmd-tries)
  (current-expected ans)
  (big-bang (new-slv-world ans)
    (to-draw render)
    (on-key key-handler)
    (stop-when done? render)
    (name "Logic Grid Puzzle Solver")))