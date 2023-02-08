#lang racket

;;;
;;; BUILDER
;;;

(require 2htdp/image
         2htdp/universe
         utils/string
         utils/states
         utils/stack
         utils/cmd-queue
         utils/2htdp/image
         (only-in "2htdp/table.rkt" current-expected puzzle-complete?)
         "solver-base.rkt"
         "2htdp/pending-stream.rkt"
         "common.rkt")

(struct bld-world slv-world (state key-tmp key keys key#
                                   stmt-tmp stmt#-tmp stmt# stmt-scr-top
                                   exp-tmp exp# exp? tbl)
  #:mutable #:transparent)

(define EVT-DEL     "\b")
(define EVT-ENTER   "\r")
(define EVT-ADD     "\t")
(define EVT-UP      "up")
(define EVT-DOWN    "down")
(define EVT-LEFT    "left")
(define EVT-RIGHT   "right")
(define EVT-ESC     "escape")
(define EVT-MENU    "m")
(define EVT-STMT    "s")
(define EVT-EXP     "e")
(define EVT-RUN     "r")
(define EVT-SEL-VAL "v")
(define EVT-SEL-NUM "n")
(define EVT-LOAD    "l")
(define EVT-CMD     " ")

(define data-file "lgps-data.txt")
(define log-file "lgps-log.txt")

(define debug? (make-parameter #f))
(define (debug-printf . args)
  (unless (false? (debug?)) (apply printf args)))

;;;
;;;; MENU
;;;;

(define (beg-menu ws state kval)
  (debug-printf "beg-menu state=~a kval=~a~%" state kval)
  (set-bld-world-key! ws 'Menu)
  (clear-stmt-flds! ws state kval)
  (unless (false? (current-log))
    (close-output-port (current-log))
    (current-log #f)))

(define (add-key-tmp ws state kval)  
  (set-bld-world-key-tmp! ws (string-append (bld-world-key-tmp ws) kval)))

(define (rem-key-tmp ws state kval) 
  (define str (bld-world-key-tmp ws))
  (define len (string-length str))
  (unless (zero? len)
    (set-bld-world-key-tmp! ws (substring str 0 (sub1 len)))))

(define (add-key-val ws state kval)
  (define val (string->expr (bld-world-key-tmp ws)))
  (define vals (hash-ref (bld-world-tbl ws) (bld-world-key ws)))
  (unless (member val vals)
    (define new-vals (append  vals (list val)))
    (hash-set! (bld-world-tbl ws) (bld-world-key ws) new-vals)
    (hash-set! (bld-world-tbl ws) val empty))
  (clear-key-flds! ws state kval))

(define (sel-key# ws state kval)
  (define n (string->number kval))
  (set-bld-world-key#! ws
                       (if (< 0 n (add1 (length (hash-ref (bld-world-tbl ws) (bld-world-key ws))))) n #f)))

(define (rem-key-val ws state kval)
  (define n (bld-world-key# ws))  
  (when (number? n)
    (define vals (hash-ref (bld-world-tbl ws) (bld-world-key ws)))
    (define val (list-ref vals (sub1 n)))
    (define new-vals (remove val vals))
    (hash-set! (bld-world-tbl ws) (bld-world-key ws) new-vals)
    (hash-remove! (bld-world-tbl ws) val))
  (clear-key-flds! ws state kval))

(define (sel-key-val ws state kval)
  (debug-printf "sel-key-val ~a ~a~%" state kval)
  (debug-printf "key=~a~%" (bld-world-key ws))
  (define n (bld-world-key# ws))
  (when (number? n)
    (define key (bld-world-key ws))
    (define vals (hash-ref (bld-world-tbl ws) key))
    (define val (list-ref vals (sub1 n)))
    (debug-printf "val=~a~%" val)
    (define ans (case key
                  [(Operator Category) val]
                  [else (cons key val)]))
    (if (false? (bld-world-exp? ws))
        (set-bld-world-stmt-tmp! ws (append (bld-world-stmt-tmp ws) (list ans)))
        (set-bld-world-exp-tmp! ws (append (bld-world-exp-tmp ws) (list ans))))
    (clear-key-flds! ws state kval)))

(define (sel-key-num ws state kval)
  (debug-printf "sel-key-num ~a ~a~%" state kval)
  (debug-printf "key=~a~%" (bld-world-key ws))
  (define n (bld-world-key# ws))
  (when (number? n)
    (define key (bld-world-key ws))
    (define vals (hash-ref (bld-world-tbl ws) key))
    (define val (list-ref vals (sub1 n)))
    (debug-printf "val=~a~%" val)
    (define ans (case key
                  [(Operator Category) val]
                  [else (cons key n)]))
    (set-bld-world-stmt-tmp! ws (append (bld-world-stmt-tmp ws) (list ans)))
    (clear-key-flds! ws state kval)))

(define (up-key-val ws state kval)
  (define n (bld-world-key# ws))
  (when (number? n)
    (define key (bld-world-key ws))
    (define ls (hash-ref (bld-world-tbl ws) key))
    (define n0 (- n 2))
    (define new-vals
      (cond
        [(negative? n0)
         (append (drop ls 1) (take ls 1))]
        [else
         (append (take ls n0)
                 (reverse (take (drop ls n0) 2))
                 (drop ls n))]))
    (hash-set! (bld-world-tbl ws) key new-vals)
    (set-bld-world-key#! ws (if (zero? (sub1 n))
                                (length (hash-ref (bld-world-tbl ws) key))
                                (sub1 n)))))

(define (down-key-val ws state kval)
  (define n (bld-world-key# ws))
  (when (number? n)
    (define key (bld-world-key ws))
    (define ls (hash-ref (bld-world-tbl ws) key))  
    (define n0 (- n 1))
    (define new-vals
      (cond
        [(zero? (- n (length ls)))
         (append (drop ls n0) (take ls n0))]
        [else
         (append (take ls n0)
                 (reverse (take (drop ls n0) 2))
                 (drop ls (add1 n)))]))
    (hash-set! (bld-world-tbl ws) key new-vals)
    (set-bld-world-key#! ws (if (> (add1 n) (length (hash-ref (bld-world-tbl ws) key)))
                                1
                                (add1 n)))))

(define (push-key ws state kval)
  (debug-printf "push-key ~a ~a~%" state kval)
  (define n (bld-world-key# ws))  
  (when (number? n)
    (clear-key-flds! ws state kval)
    (define keys (bld-world-keys ws))
    (define curr (bld-world-key ws))
    (define vals (hash-ref (bld-world-tbl ws) curr))
    (define next (list-ref vals (sub1 n)))
    (define id (case next
                 [(Category) 'cat]
                 [(Operator) 'oper]
                 [else (State-id state)]))
    (define task-id (if (eq? curr 'Category) 'input 'edit))
    (debug-printf "curr=~a next=~a id=~a task-id=~a~%" curr next id task-id)
    (case curr
      [(Menu)
       (set-bld-world-key! ws next)
       (set-bld-world-keys! ws (cons curr (bld-world-keys ws)))]
      [(Category)
       (set-bld-world-key! ws next)
       (set-bld-world-keys! ws (cons curr (bld-world-keys ws)))]
      [else (void)])
    (raise (cons id task-id))))

(define (pop-key ws state kval)
  (debug-printf "pop-key ~a ~a~%" state kval)
  (define keys (bld-world-keys ws))
  (unless (empty? keys)
    (define key (car keys))
    (set-bld-world-key! ws key)
    (set-bld-world-keys! ws (cdr keys))
    (clear-key-flds! ws state kval)
    (define id (case key
                 [(Category) 'cat]
                 [(Operator) 'oper]
                 [else 'menu]))
    (debug-printf "id=~a~%" id)
    (raise (cons id 'edit))))

(define (clear-key-flds! ws state kval)
  (set-bld-world-key-tmp! ws "")
  (set-bld-world-key#! ws #f)
  (set-slv-world-message! ws #f))

;;;
;;; STMT
;;;

(define (beg-stmt ws state kval)
  (debug-printf "beg-stmt state=~a kval=~a~%" state kval)
  (set-bld-world-key! ws 'Menu)
  (set-bld-world-exp?! ws #f)
  (clear-key-flds! ws state kval))

(define (stmt->string #:colon? (colon? #f) val)
  (define vstr (~a val))
  (define str (substring vstr 0 (sub1 (string-length vstr))))
  (define vals (append '(("((" "[(") ("))" ")]"))
                       (if colon?
                           '((" . " " : "))
                           '())))
  (string-append
   (for/fold ([val (~a str)])
             ([args vals])
     (string-replace val (first args) (second args)))
   ")"))

(define (rem-stmt-tmp ws state kval) 
  (define vals (bld-world-stmt-tmp ws))
  (unless (empty? vals)
    (set-bld-world-stmt-tmp! ws (drop-right vals 1))))

(define (string->stmt str)
  (define vals '(("[(" "((") (")]" "))") (" : " " . ")))
  (for/fold ([val (~a str)])
            ([args vals])
    (string-replace val (first args) (second args))))

(define (normalize-statement oper . args)
  (debug-printf "normalize-statement ~a ~a~%" oper args)  
  (define len (length args))
  (case oper
    [(relate!)
     (case len
       [(2) (cons oper args)]
       [else (raise-user-error "Invalid statement syntax.")])]
    [(|relate! #f|)
     (case len
       [(2) (append (string->expr (format "(~a)" oper)) args)]
       [else (raise-user-error "Invalid statement syntax.")])]
    [(distinct!) (cons oper args)]
    [(xor!)
     (case len
       [(3) (list oper (first args) (list (second args) (third args)))]
       [(4) (list oper
                  (list (first args) (second args))
                  (list (third args) (fourth args)))]
       [else (raise-user-error "Statement invalid syntax.")])]
    [(criss-cross!)
     (case len
       [(4) (list oper
                  (list (first args) (second args))
                  (list (third args) (fourth args)))]
       [else (raise-user-error "Statement invalid syntax.")])]
    [(seq!)
     (cons oper args)]
    [else (raise-user-error "Statement invalid syntax.")]))

(define (add-stmt-val ws state kval)
  (with-handlers ([exn:fail? (λ (e) (set-slv-world-message! ws (exn-message e))
                               (raise '(stmt . error)))])
    (define tmp (apply normalize-statement (bld-world-stmt-tmp ws)))
    (define val tmp)
    (define vals (hash-ref (bld-world-tbl ws) 'Statement))
    (unless (member val vals)
      (define new-vals (append  vals (list val)))
      (hash-set! (bld-world-tbl ws) 'Statement new-vals))
    (clear-stmt-flds! ws state kval)))

(define (sel-stmt# ws state kval)
  (debug-printf "sel-stmt# ~a ~a~%" state kval)
  (define str (string-append (bld-world-stmt#-tmp ws) kval))
  (set-bld-world-stmt#-tmp! ws (case (string-length str)
                                 [(3) (substring str 1)]
                                 [else str]))
  (debug-printf "stmt#-tmp=~a~%" (bld-world-stmt#-tmp ws))
  (define n (string->number (bld-world-stmt#-tmp ws)))
  (define top (bld-world-stmt-scr-top ws))
  (define key 'Statement)
  (define len (length (hash-ref (bld-world-tbl ws) key)))
  (define btm (min len  (+ top STMT-SCR-MAX)))
  (define rng (range (add1 top) (add1 btm)))
  (debug-printf "n=~a rng=~a~%" n rng)
  (cond
    [(member n rng) (set-bld-world-stmt#! ws n)]
    [else
     (set-bld-world-stmt#! ws
                           (case (string-length (bld-world-stmt#-tmp ws))
                             [(2) (set-bld-world-stmt#-tmp! ws (substring (bld-world-stmt#-tmp ws) 1))
                                  (define n (string->number (bld-world-stmt#-tmp ws)))
                                  (if (member n rng) n #f)]
                             [else #f]))]))

(define (rem-stmt-val ws state kval)
  (define n (bld-world-stmt# ws))  
  (when (number? n)
    (define key 'Statement)
    (define vals (hash-ref (bld-world-tbl ws) key))
    (define val (list-ref vals (sub1 n)))
    (define new-vals (remove val vals))
    (hash-set! (bld-world-tbl ws) key new-vals))
  (clear-stmt-flds! ws state kval))

(define (up-stmt-val ws state kval)
  (define n (bld-world-stmt# ws))
  (cond
    [(number? n)
     (define key 'Statement)
     (define ls (hash-ref (bld-world-tbl ws) key))
     (define n0 (- n 2))
     (define new-vals
       (cond
         [(negative? n0)
          (append (drop ls 1) (take ls 1))]
         [else
          (append (take ls n0)
                  (reverse (take (drop ls n0) 2))
                  (drop ls n))]))
     (hash-set! (bld-world-tbl ws) key new-vals)
     (set-bld-world-stmt#! ws (if (zero? (sub1 n))
                                  (length (hash-ref (bld-world-tbl ws) key))
                                  (sub1 n)))]
    [else
     (define n (- (bld-world-stmt-scr-top ws) (add1 STMT-SCR-MAX)))
     (set-bld-world-stmt-scr-top! ws
                                  (cond
                                    [(negative? n) 0]
                                    [else n]))]))

(define (down-stmt-val ws state kval)
  (define n (bld-world-stmt# ws))
  (cond
    [(number? n)
     (define key 'Statement)
     (define ls (hash-ref (bld-world-tbl ws) key))  
     (define n0 (- n 1))
     (define new-vals
       (cond
         [(zero? (- n (length ls)))
          (append (drop ls n0) (take ls n0))]
         [else
          (append (take ls n0)
                  (reverse (take (drop ls n0) 2))
                  (drop ls (add1 n)))]))
     (hash-set! (bld-world-tbl ws) key new-vals)
     (set-bld-world-stmt#! ws (if (> (add1 n) (length (hash-ref (bld-world-tbl ws) key)))
                                  1
                                  (add1 n)))]
    [else
     (define top (bld-world-stmt-scr-top ws))
     (define len (length (hash-ref (bld-world-tbl ws) 'Statement)))
     (define n (+ top STMT-SCR-MAX))
     (when (< n len)
       (set-bld-world-stmt-scr-top! ws n))]))

(define (clear-stmt-flds! ws state kval)
  (set-bld-world-stmt-tmp! ws '())
  (set-bld-world-stmt#! ws #f)
  (set-bld-world-stmt#-tmp! ws "")
  (set-slv-world-message! ws #f))

;;;
;;; exp
;;;

(define (beg-exp ws state kval)
  (debug-printf "beg-exp state=~a kval=~a~%" state kval)
  (set-bld-world-key! ws 'Menu)
  (set-bld-world-exp?! ws #t)
  (clear-key-flds! ws state kval))

(define (rem-exp-tmp ws state kval) 
  (define vals (bld-world-exp-tmp ws))
  (unless (empty? vals)
    (set-bld-world-exp-tmp! ws (drop-right vals 1))))

(define (exp->string #:colon? (colon? #f) val)
  (define str (~a val))
  (define args-list (append '(("((" "[(") ("))" ")]"))
                            (if colon? (" . " " : ") '())))
  (for/fold ([val (~a str)])
            ([args args-list])
    (string-replace val (first args) (second args))))

(define (string->exp str)
  (for/fold ([val (~a str)])
            ([args '((" : " " . ") ("[(" "((") (")]" "))"))])
    (string-replace val (first args) (second args))))

(define (normalize-Expected . args)
  args)

(define (add-exp-val ws state kval)
  (define key 'Expected)
  (with-handlers ([exn:fail? (λ (e) (set-slv-world-message! ws (exn-message e))
                               (raise '(exp . error)))])
    (define tmp (apply normalize-Expected (bld-world-exp-tmp ws)))
    (define val tmp)
    (define vals (hash-ref (bld-world-tbl ws) key))
    (unless (member val vals)
      (define new-vals (append  vals (list val)))
      (hash-set! (bld-world-tbl ws) key new-vals))
    (clear-exp-flds! ws state kval)))

(define (sel-exp# ws state kval)
  (define n (string->number kval))
  (define key 'Expected)
  (set-bld-world-exp#! ws
                       (if (< 0 n (add1 (length (hash-ref (bld-world-tbl ws) key)))) n #f)))

(define (rem-exp-val ws state kval)
  (define n (bld-world-exp# ws))  
  (when (number? n)
    (define key 'Expected)
    (define vals (hash-ref (bld-world-tbl ws) key))
    (define val (list-ref vals (sub1 n)))
    (define new-vals (remove val vals))
    (hash-set! (bld-world-tbl ws) key new-vals))
  (clear-exp-flds! ws state kval))

(define (up-exp-val ws state kval)
  (define n (bld-world-exp# ws))
  (when (number? n)
    (define key 'Expected)
    (define ls (hash-ref (bld-world-tbl ws) key))
    (define n0 (- n 2))
    (define new-vals
      (cond
        [(negative? n0)
         (append (drop ls 1) (take ls 1))]
        [else
         (append (take ls n0)
                 (reverse (take (drop ls n0) 2))
                 (drop ls n))]))
    (hash-set! (bld-world-tbl ws) key new-vals)
    (set-bld-world-exp#! ws (if (zero? (sub1 n))
                                (length (hash-ref (bld-world-tbl ws) key))
                                (sub1 n)))))

(define (down-exp-val ws state kval)
  (define n (bld-world-exp# ws))
  (when (number? n)
    (define key 'Expected)
    (define ls (hash-ref (bld-world-tbl ws) key))  
    (define n0 (- n 1))
    (define new-vals
      (cond
        [(zero? (- n (length ls)))
         (append (drop ls n0) (take ls n0))]
        [else
         (append (take ls n0)
                 (reverse (take (drop ls n0) 2))
                 (drop ls (add1 n)))]))
    (hash-set! (bld-world-tbl ws) key new-vals)
    (set-bld-world-exp#! ws (if (> (add1 n) (length (hash-ref (bld-world-tbl ws) key)))
                                1
                                (add1 n)))))

(define (clear-exp-flds! ws state kval)
  (set-bld-world-exp-tmp! ws '())
  (set-bld-world-exp#! ws #f))

(define (toggle-exp? ws state kval)
  (define exp? (bld-world-exp? ws))
  (define new-exp? (not exp?))
  (set-bld-world-exp?! ws new-exp?)
  (case new-exp?
    [(#t) (clear-stmt-flds! ws state kval)
          (raise (cons 'exp 'edit))]
    [else (clear-exp-flds! ws state kval)
          (raise (cons 'stmt 'edit))]))

;;;
;;; GO
;;;

(define (beg-solve ws state kval)
  (with-handlers ([exn:fail? (λ (e) (set-slv-world-message! ws (exn-message e))
                               (raise '(menu . error)))])
    (validate-puzzle ws state kval))
  ;; Create the init values for the solver table.
  (define out (open-output-file data-file #:exists 'replace))
  (displayln "" out)
  (displayln "(init" out)
  (define inits
    (for/list ([cat (hash-ref (bld-world-tbl ws) 'Category)])
      (define init (cons cat (hash-ref (bld-world-tbl ws) cat)))
      (define str (format "\t~a" init))
      (displayln str out)
      init))
  (displayln ")" out)
  (displayln "" out)
  (displayln "" out)
  ;; Create stmts for the solver.
  (define stmts
    (for/list ([stmt (hash-ref (bld-world-tbl ws) 'Statement)])
      (define str (string->stmt stmt))
      (displayln str out)
      stmt))
  (displayln "" out)
  (displayln "(go!" out)
  (define exp
    (for/list ([exp (hash-ref (bld-world-tbl ws) 'Expected)])
      (define str (string->exp exp))
      (displayln str out)
      exp))
  (displayln ")" out)
  (close-output-port out)
  ;; Create puzzle 
  (apply λinit inits)
  ;; Initialize stacks.
  (clear! pending)
  (clear! failed)
  (clear! successful)
  (for ([stmt stmts])
    (apply add-to-pending
           (case (car stmt)
             [(relate!) (append (list 1 λrelate!) (cdr stmt))]
             [(distinct!) (append (list 1 λdistinct!) (cdr stmt))]
             [(xor!)
              (define s1 (first (cdr stmt)))
              (define s2 (second (cdr stmt)))
              (append (list #f λxor!)
                      (case (list? s1)
                        [(#f)
                         (list [list s1 (first s2)]
                               [list s1 (second s2)])]
                        [else (cdr stmt)]))]
             [(criss-cross!) (append (list #f λcriss-cross!)
                                     (first (cdr stmt))
                                     (second (cdr stmt)))]
             [(seq!) (append (list #f λseq!) (cdr stmt))]
             [else (error (format "~a: ~a not a valid form." (car stmt) (cdr stmt)))])))
  ;; Initialize command tries for pending commands.
  (initialize-cmd-tries)
  ;; Populate the exp for validation.  
  (current-expected (hash-ref (bld-world-tbl ws) 'Expected))
  ;; Open the log file.
  (current-log (open-output-file log-file #:exists 'replace)))

(define (validate-puzzle ws state kval)
  (printf "validate-puzzle ~a ~a~%" state kval)
  (define tbl (bld-world-tbl ws))
  (printf "tbl=~a~%" tbl)
  (define cats (hash-ref tbl 'Category))
  (printf "cats=~a~%" cats)
  (cond
    [(empty? cats)
     (error "Categories and properties not populated.")]
    [(< (length cats) 2)
     (error "Puzzles require 2 or more categories.")]
    [else
     (for/fold ([prev-cat #f]
                [prev-len 0])
               ([c cats])
       (define len (length (hash-ref tbl c)))
       (cond
         [(zero? len) (error (format "Cateroy ~a has no properties." c))]
         [(zero? prev-len) (values c len)]
         [(< len prev-len) (error (format "Cateroy ~a has  less properties than ~a." c prev-cat))]
         [(> len prev-len) (error (format "Cateroy ~a has  more properties than ~a." c prev-cat))]
         [else (values c len)]))]))

(define (load-init tbl inits)
  (debug-printf "load-init~%")
  (hash-set! tbl 'Category (map car inits))
  (for ([init inits])
    (hash-set! tbl (car init) (cdr init))))

(define (load-exp tbl exp)
  (hash-set! tbl 'Expected exp))

(define (load-stmts tbl stmts)
  (hash-set! tbl 'Statement stmts))

(define (read-file ws state kval)
  (debug-printf "read-file ~a ~a~%" state kval)
  (define tbl (bld-world-tbl ws))
  (with-handlers ([exn:fail:filesystem:errno? (λ (e) (printf "~a~%Loading bypassed.~%" (exn-message e)))])
    (define in (open-input-file data-file #:mode 'text))
    (define stmts
      (reverse (for/fold ([stmts empty])
                         ([v (range 20)])
                 (define val (read in))
                 (debug-printf "~a~%" val)
                 #:break (eof-object? val)
                 (case (car val)
                   [(init) (load-init tbl (rest val)) stmts]
                   [(go!) (load-exp tbl (rest val)) stmts]
                   [else (cons val stmts)]))))
    (load-stmts tbl stmts)
    (close-input-port in)))

(define (process-cmd ws state kval)
  (unless (and (zero? (size-active pending))
               (zero? (size-active failed)))
    (stream-pending)
    (set-slv-world-count! ws (add1 (slv-world-count ws)))
    (when (puzzle-complete? ws (? 'cs))
        (set-slv-world-message! ws "Woot! Puzzle completed!")
      (raise '(solve . msg)))))

(define (clear-solve-flds! ws state kval)  
  (set-slv-world-message! ws #f))

(define STATES
  (states-hash
   (menu
    (edit EVT-RIGHT       push-key         menu edit)
    (edit EVT-STMT        beg-stmt         stmt input)
    (edit EVT-EXP        beg-exp           exp input)
    (edit EVT-ESC         #F               stmt input)
    (edit EVT-RUN         beg-solve        solve edit)
    (edit EVT-LOAD        read-file        menu edit)
    (edit string->number  sel-key#         menu edit)

    (error EVT-ESC         clear-key-flds! menu edit))

   (oper    
    (edit EVT-ESC         clear-key-flds! oper edit)
    (edit EVT-RIGHT       push-key        oper edit)
    (edit EVT-LEFT        pop-key         oper edit)
    (edit EVT-SEL-VAL     sel-key-val     oper edit)
    (edit string->number  sel-key#        oper edit)
    (edit EVT-STMT        beg-stmt        stmt input)
    (edit EVT-EXP        beg-exp        exp input))

   (cat
    (input EVT-DEL        rem-key-tmp        cat input)
    (input EVT-ENTER      add-key-val        cat input)
    (input EVT-ESC        #F                 cat edit)
    (input string?        add-key-tmp        cat input)
    (input EVT-STMT       beg-stmt           stmt input)
    (input EVT-EXP       beg-exp             exp input)

    (edit EVT-ADD         clear-key-flds!    cat input)
    (edit EVT-DEL         rem-key-val        cat edit)
    (edit EVT-UP          up-key-val         cat edit)
    (edit EVT-DOWN        down-key-val       cat edit)
    (edit EVT-ESC         clear-key-flds!    cat edit)
    (edit EVT-RIGHT       push-key           cat input)
    (edit EVT-LEFT        pop-key            cat edit)
    (edit EVT-SEL-VAL     sel-key-val        cat edit)
    (edit EVT-SEL-NUM     sel-key-num        cat edit)
    (edit EVT-STMT        beg-stmt           stmt input)
    (edit EVT-EXP        beg-exp           exp input)
    (edit string->number  sel-key#           cat edit))
   
   (stmt
    (input EVT-DEL        rem-stmt-tmp       stmt input)
    (input EVT-ENTER       add-stmt-val      stmt edit)
    (input EVT-MENU         beg-menu         menu edit)
    (input EVT-EXP         beg-exp         exp input)
    (input EVT-ESC          clear-stmt-flds! stmt edit)

    (edit EVT-ADD          clear-stmt-flds!  stmt input)
    (edit EVT-DEL          rem-stmt-val      stmt edit)
    (edit EVT-UP           up-stmt-val       stmt edit)
    (edit EVT-DOWN         down-stmt-val     stmt edit)
    (edit EVT-MENU         beg-menu                menu edit)
    (edit EVT-EXP        beg-exp           exp input)
    (edit EVT-ESC          clear-stmt-flds!  stmt edit)
    (edit EVT-LEFT         toggle-exp?      stmt edit)
    (edit EVT-RIGHT        toggle-exp?      stmt edit)
    (edit string->number   sel-stmt#         stmt edit)

    (error EVT-ESC         clear-stmt-flds!  stmt input))

   (exp
    (input EVT-DEL         rem-exp-tmp       exp input)
    (input EVT-ENTER       add-exp-val       exp edit)
    (input EVT-MENU        beg-menu          menu edit)
    (input EVT-STMT        beg-stmt          stmt edit)
    (input EVT-ESC         clear-exp-flds!  exp edit)

    (edit EVT-ADD          clear-exp-flds!  exp input)
    (edit EVT-DEL          rem-exp-val      exp edit)
    (edit EVT-UP           up-exp-val       exp edit)
    (edit EVT-DOWN         down-exp-val     exp edit)
    (edit EVT-MENU         beg-menu          menu edit)
    (edit EVT-STMT         beg-stmt          stmt edit)
    (edit EVT-ESC          clear-exp-flds!  exp edit)
    (edit EVT-LEFT         toggle-exp?      exp edit)
    (edit EVT-RIGHT        toggle-exp?      exp edit)
    (edit string->number   sel-exp#         exp edit)

    (error EVT-ESC         clear-exp-flds!  exp edit))

   (solve    
    (edit EVT-CMD          process-cmd       solve edit)
    (edit EVT-MENU         beg-menu          menu edit)

    (msg  EVT-ESC         clear-solve-flds!  solve edit))))

;; Translate numpad values into equivalent non-numpad values. 
(define (key-normalize state ke)
  (define id (State-id state))
  (define task-id (State-task-id state))
  (define kval
    (cond
      [(key=? ke "numpad-enter") EVT-ENTER]
      [(and (or (eq? id 'cat) (eq? id 'prop)) (eq? task-id 'input) (key=? ke " ")) "-"]
      [else (string-trim (string-downcase ke) "numpad")]))  
  kval)

(define (key-handler ws ke)
  (debug-printf "key-handler state=~a ke=~a~%" (bld-world-state ws) ke)
  (state-handler ws STATES (bld-world-state ws) (key-normalize (bld-world-state ws) ke))
  ws)

(define STMT-SCR-MAX 10)

(define MT-WIDTH 1200)
(define MT-HEIGHT 650)
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
                       (text (~a val) FONT-SIZE FONT-COLOR)
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
  (define stmts (hash-ref (bld-world-tbl ws) 'Statement))
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
    (for/list ([val (hash-ref (bld-world-tbl ws) 'Expected)]
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
                                   (text (~a (bld-world-key ws)) FONT-SIZE TITLE-COLOR)
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
  (define tbl (make-hash '((Category)
                           (Statement)
                           (Expected)
                           (Menu Operator Category)                           
                           (Operator relate! |relate! #f| distinct! xor! criss-cross! seq!))))
  (bld-world 0 #f
             (State 'menu 'edit "") "" 'Menu empty #f
             '() "" #f 0
             '() #f #f tbl))

(big-bang (new-world)
  (to-draw render)
  (on-key key-handler)
  (name "Logic Grid Puzzle Builder"))