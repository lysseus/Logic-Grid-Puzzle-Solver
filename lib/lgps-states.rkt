#lang racket

;;;
;;; LGPS-STATES
;;;

(provide STATES key-normalize)

(require utils/states
         utils/cmd-queue
         utils/string
         utils/stack
         "2htdp/table.rkt"
         "common.rkt"
         "init.rkt"
         "queries.rkt"
         "statements.rkt")

(define EVT-DEL     "\b")
(define EVT-ENTER   "\r")
(define EVT-ADD     "\t")
(define EVT-UP      "up")
(define EVT-DOWN    "down")
(define EVT-LEFT    "left")
(define EVT-RIGHT   "right")
(define EVT-ESC     "escape")
(define EVT-MAIN    "m")
(define EVT-STMT    "s")
(define EVT-EXP     "e")
(define EVT-RUN     "r")
(define EVT-SEL-VAL "v")
(define EVT-SEL-NUM "n")
(define EVT-LOAD    "l")
(define EVT-CMD     " ")


;; Translate numpad values into equivalent non-numpad values. 
(define (key-normalize state ke)
  (define id (State-id state))
  (define task-id (State-task-id state))
  (define kval
    (cond
      [(string=? ke "numpad-enter") EVT-ENTER]
      [(and (or (eq? id 'cat) (eq? id 'prop)) (eq? task-id 'input) (string=? ke " ")) "-"]
      [(member ke '("lshift" "rshift" "control" "rcontrol" "end" "next" "home" "prior")) ""]
      [else (string-trim (string-downcase ke) "numpad")]))  
  kval)

(define data-file "lgps-data.txt")
(define log-file "lgps-log.txt")

(define debug? (make-parameter #f))
(define (debug-printf . args)
  (unless (false? (debug?)) (apply printf args)))

;;;
;;;; MAIN
;;;;

(define (beg-main ws state kval)
  (debug-printf "beg-main state=~a kval=~a~%" state kval)
  (set-bld-world-key! ws '%Main)  
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
  (define val (string->expr (string-titlecase (bld-world-key-tmp ws))))
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
                  [(%Operator %Category) val]
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
                  [(%Operator %Category) val]
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
                 [(%Category) 'cat]
                 [(%Operator) 'oper]
                 [else (State-id state)]))
    (define task-id 'edit)    
    (debug-printf "curr=~a next=~a id=~a task-id=~a~%" curr next id task-id)
    (case curr
      [(%Main)
       (set-bld-world-key! ws next)
       (set-bld-world-keys! ws (cons curr (bld-world-keys ws)))]
      [(%Category)
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
                 [(%Category) 'cat]
                 [(%Operator) 'oper]
                 [else 'main]))
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
  (set-bld-world-key! ws '%Main)
  (set-bld-world-exp?! ws #f)
  (clear-key-flds! ws state kval))

(define (rem-stmt-tmp ws state kval) 
  (define vals (bld-world-stmt-tmp ws))
  (unless (empty? vals)
    (set-bld-world-stmt-tmp! ws (drop-right vals 1))))

(define (string->stmt str)
  (define vals '(("[(" "((") (")]" "))") (" : " " . ")))
  (for/fold ([val (~a str)])
            ([args vals])
    (string-replace val (first args) (second args))))

;; Returns true if the value is a valid statment category/property pair.
;; Otherise returns false.
(define (category/property-pair? v)
  (and (pair? v)
       (not (list? v))
       (symbol? (car v))
       (or (symbol? (cdr v)) (number? (cdr v)))))

;; Returns true if the value is a valid statment category/offset pair.
;; Otherwise returns false.
(define (category/offset-pair? v)
  (and (pair? v)
       (not (list? v))
       (symbol? (car v))
       (natural? (cdr v))))


(define OPERATOR-ERROR "Operator not valid.")
(define ARGUMENT-NUMBER-ERROR "Wrong number of arguments for this operator.")
(define ARGUMENT-TYPE-ERROR "Arguments must be category/property pairs.")
(define ARGUMENT-FIRST-TYPE-ERROR "1st argument must be a sequence-category or sequence-category/offset pair.")
(define ARGUMENT-FIRST-CATEGORY-ERROR "1st argument must be a sequence-category.")
(define ARGUMENT-REST-TYPE-ERROR "Arguments after 1st must be category/property pairs.")
(define ARGUMENT-DUPLICATE-ERROR "Agument duplicates.")
(define CATEGORY-PROPERTY-ILLOGICAL-ERROR "Illogical duplicate category/property pairs.")
(define CLAUSE-SET-DUPLICATE-ERROR "Clause sets are duplicates.")
(define CLAUSE-SET-CATEGORY-DUPLICATE-ERROR "Clause sets catagories are duplicates.")

(define CATEGORY-DUPLICATE-ERROR "Category duplicates.")
(define CATEGORY-MAJORITY-DUPLICATE-ERROR "More than half the categories are duplicates.")
(define CATEGORY-ALL-DUPLICATE-ERROR "Categories are all duplicates.")
(define CATEGORY-1+2-DUPLICATE-ERROR "Category 1 and 2 are duplicates.")
(define CATEGORY-1+3-DUPLICATE-ERROR "Category 1 and 3 are duplicates.")
(define CATEGORY-3+4-DUPLICATE-ERROR "Category 3 and 4 are duplicates.")
(define SEQUENCE-CATEGORY-ERROR "Sequence-category found in category/property pairs.")

;; Validates syntactically the new Statement entry before committing it.I
;; Each operator has its own procedure for validation and formats a succcessful
;; statment syntax into a list appropriate for its execution function.
(define (normalize-statement oper . args)
  (debug-printf "normalize-statement ~a ~a~%" oper args)  
  (define len (length args))
  (case oper
    [(relate!)      (normalize-relate! len oper args)]
    [(|relate! #f|) (normalize-distinct! len oper args)]
    [(distinct!)    (normalize-distinct! len oper args)]
    [(xor!)         (normalize-xor! len oper args)]
    [(criss-cross!) (normalize-criss-cross! len oper args)]
    [(seq!)         (normalize-seq! len oper args)]
    [(next!)        (normalize-next! len oper args)]
    [else (raise-user-error OPERATOR-ERROR)]))

;; Validates the relate! arguments and returns a list representation
;; of the statement, or raises an issue if the syntax is invalid for the
;; operator.
(define (normalize-relate! len oper args)
  (case len
    [(2)
     (define cp1 (first args))
     (define cp2 (second args))
     (cond       
       [(and (category/property-pair? cp1) (category/property-pair? cp2))
        (define cat1 (car cp1))
        (define cat2 (car cp2))
        (cond
          [(eq? cat1 cat2)
           (raise-user-error CATEGORY-DUPLICATE-ERROR)]
          [else (cons oper args)])]
       [else (raise-user-error ARGUMENT-TYPE-ERROR)])]
    [else (raise-user-error ARGUMENT-NUMBER-ERROR)]))

;; Validates the distinct! arguments and returns a list representation
;; of the statement, or raises an issue if the syntax is invalid for the
;; operator.
(define (normalize-distinct! len oper args)
  (case len
    [(0 1)
     (raise-user-error ARGUMENT-NUMBER-ERROR)]
    [else
     (define cps? (andmap category/property-pair? args))
     (define cps (if cps?
                     (remove-duplicates args)
                     '()))
     (define cs (remove-duplicates (map car cps)))
     (cond
       ;; Not all category/pairs.
       [(false? cps?)        
        (raise-user-error ARGUMENT-TYPE-ERROR)]
       ;; Found duplicates.
       [(> (length args) (length cps))
        (raise-user-error ARGUMENT-DUPLICATE-ERROR)]
       ;; Categories all the same.
       [(= (length cs) 1)
        (raise-user-error CATEGORY-ALL-DUPLICATE-ERROR)]       
       [else (cons oper args)])]))

(define (normalize-xor! len oper args)
  (case len
    ;; A is either B or C.
    [(3) (normalize-xor!-3 len oper args)]
    ;; Either A is B or C is D.
    [(4) (normalize-xor!-4 len oper args)]
    [else
     (raise-user-error ARGUMENT-NUMBER-ERROR)]))

;; Validates the xor! 3 arguments and returns a list representation
;; of the statement, or raises an issue if the syntax is invalid for the
;; operator.
(define (normalize-xor!-3 len oper args)
  (define cps? (andmap category/property-pair? args))
  (define cps (if cps?
                  (remove-duplicates args)
                  '()))
  (define cs-1+2 (if cps?
                     (remove-duplicates (take (map car cps) 2))
                     '()))
  (define cs-1+3 (if cps?
                     (remove-duplicates (append (take (map car cps) 1)
                                                (drop (map car cps) 2)))
                     '()))
  (cond
    ;; Not all category/pairs.
    [(false? cps?)        
     (raise-user-error ARGUMENT-TYPE-ERROR)]
    ;; Found duplicates.
    [(> len (length cps))
     (raise-user-error ARGUMENT-DUPLICATE-ERROR)]
    ;; Categories all the same.
    [(= (length cs-1+2) 1)
     (raise-user-error CATEGORY-1+2-DUPLICATE-ERROR)]
    [(= (length cs-1+3) 1)
     (raise-user-error CATEGORY-1+3-DUPLICATE-ERROR)]
    [else (list oper (first args) (list (second args) (third args)))]))

;; Validates the xor! 4 arguments and returns a list representation
;; of the statement, or raises an issue if the syntax is invalid for the
;; operator.
(define (normalize-xor!-4 len oper args)
  (define cps? (andmap category/property-pair? args))
  (define cps (if cps?
                  args
                  '()))
  (define cps-1+2 (if cps?
                      (take cps 2)
                      '()))
  (define cps-3+4 (if cps?
                      (drop cps 2)
                      '()))
  (define cs-1+2 (if cps?
                     (remove-duplicates (map car cps-1+2))
                     '()))
  (define cs-3+4 (if cps?
                     (remove-duplicates (map car cps-3+4))
                     '()))
  (cond
    ;; Not all category/pairs.
    [(false? cps?)        
     (raise-user-error ARGUMENT-TYPE-ERROR)]
    ;; Found duplicates.
    [(> len (length cps))
     (raise-user-error ARGUMENT-DUPLICATE-ERROR)]
    ;; Categories all the same.
    [(= (length cs-1+2) 1)
     (raise-user-error CATEGORY-1+2-DUPLICATE-ERROR)]
    [(= (length cs-3+4) 1)
     (raise-user-error CATEGORY-3+4-DUPLICATE-ERROR)]
    [(empty? (remove* cps-1+2 cps-3+4))
     (raise-user-error CLAUSE-SET-DUPLICATE-ERROR)]
    [else (list oper
                (list (first args) (second args))
                (list (third args) (fourth args)))]))

;; Validates the criss-cross! arguments and returns a list representation
;; of the statement, or raises an issue if the syntax is invalid for the
;; operator.
(define (normalize-criss-cross! len oper args)
  (case len    
    [(4) (define (max-category-count cps)
           (define cs (map car cps))
           (apply max (for/list ([c cs])
                        (count (λ (v) (eq? v c)) cs))))
         (define cps? (andmap category/property-pair? args))         
         
         ;; Not all category/pairs.
         (when (false? cps?)        
           (raise-user-error ARGUMENT-TYPE-ERROR))

         ;; Look for duplicate arguments.
         (when (or (< (length (remove-duplicates args))
                      (length args)))
           (raise-user-error ARGUMENT-DUPLICATE-ERROR))
                  
         ;; Look for duplicate categories.
         (when (= (max-category-count args) 3)
           (raise-user-error CLAUSE-SET-CATEGORY-DUPLICATE-ERROR))
         
         (list oper
               (list (first args) (second args))
               (list (third args) (fourth args)))]
    [else
     (raise-user-error ARGUMENT-NUMBER-ERROR)]))

;; Validates the seq! arguments and returns a list representation
;; of the statement, or raises an issue if the syntax is invalid for the
;; operator.
(define (normalize-seq! len oper args)
  (case len
    [(1 2)
     (raise-user-error ARGUMENT-NUMBER-ERROR)]
    [else (cond
            [(not (or (symbol? (first args)) (category/offset-pair? (first args))))
             (raise-user-error ARGUMENT-FIRST-TYPE-ERROR)]
            [(not (andmap category/property-pair? (rest args)))
             (raise-user-error ARGUMENT-REST-TYPE-ERROR)]                      
            [(> len (length (remove-duplicates args)))
             (raise-user-error ARGUMENT-DUPLICATE-ERROR)]
            [(and (symbol? (first args))
                  (member (first args) (map car (rest args))))
             (raise-user-error SEQUENCE-CATEGORY-ERROR)]
            [(and (category/offset-pair? (first args))
                  (member (car (first args)) (map car (rest args))))
             (raise-user-error SEQUENCE-CATEGORY-ERROR)]
            [else (cons oper args)])]))

;; Validates the next! arguments and returns a list representation
;; of the statement, or raises an issue if the syntax is invalid for the
;; operator.
(define (normalize-next! len oper args)
  (case len
    [(3) (cond
           [(not (symbol? (first args)))
            (raise-user-error ARGUMENT-FIRST-CATEGORY-ERROR)]
           [(not (andmap category/property-pair? (rest args)))
            (raise-user-error ARGUMENT-REST-TYPE-ERROR)]                      
           [(> len (length (remove-duplicates args)))
            (raise-user-error ARGUMENT-DUPLICATE-ERROR)]
           [(and (symbol? (first args))
                 (member (first args) (map car (rest args))))
            (raise-user-error SEQUENCE-CATEGORY-ERROR)]           
           [else (cons oper args)])]
    [else
     (raise-user-error ARGUMENT-NUMBER-ERROR)]))

(define (add-stmt-val ws state kval)
  (with-handlers ([exn:fail? (λ (e) (set-slv-world-message! ws (exn-message e))
                               (raise '(stmt . error)))])
    (define tmp (apply normalize-statement (bld-world-stmt-tmp ws)))
    (define val tmp)
    (define vals (hash-ref (bld-world-tbl ws) '%Statement))
    (unless (member val vals)
      (define new-vals (append  vals (list val)))
      (hash-set! (bld-world-tbl ws) '%Statement new-vals))
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
  (define key '%Statement)
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
    (define key '%Statement)
    (define vals (hash-ref (bld-world-tbl ws) key))
    (define val (list-ref vals (sub1 n)))
    (define new-vals (remove val vals))
    (hash-set! (bld-world-tbl ws) key new-vals))
  (clear-stmt-flds! ws state kval))

(define (up-stmt-val ws state kval)
  (define n (bld-world-stmt# ws))
  (cond
    [(number? n)
     (define key '%Statement)
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
     (define key '%Statement)
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
     (define len (length (hash-ref (bld-world-tbl ws) '%Statement)))
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
  (set-bld-world-key! ws '%Main)
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
  (define key '%Expected)
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
  (define key '%Expected)
  (set-bld-world-exp#! ws
                       (if (< 0 n (add1 (length (hash-ref (bld-world-tbl ws) key)))) n #f)))

(define (rem-exp-val ws state kval)
  (define n (bld-world-exp# ws))  
  (when (number? n)
    (define key '%Expected)
    (define vals (hash-ref (bld-world-tbl ws) key))
    (define val (list-ref vals (sub1 n)))
    (define new-vals (remove val vals))
    (hash-set! (bld-world-tbl ws) key new-vals))
  (clear-exp-flds! ws state kval))

(define (up-exp-val ws state kval)
  (define n (bld-world-exp# ws))
  (when (number? n)
    (define key '%Expected)
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
    (define key '%Expected)
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
                               (raise '(main . error)))])
    (validate-puzzle ws state kval))
  ;; Create the init values for the solver table.
  (define out (open-output-file data-file #:exists 'replace))
  (displayln "" out)
  (displayln "(init" out)
  (define inits
    (for/list ([cat (hash-ref (bld-world-tbl ws) '%Category)])
      (define init (cons cat (hash-ref (bld-world-tbl ws) cat)))
      (define str (format "\t~a" init))
      (displayln str out)
      init))
  (displayln ")" out)
  (displayln "" out)
  (displayln "" out)
  ;; Create stmts for the solver.
  (define stmts
    (for/list ([stmt (hash-ref (bld-world-tbl ws) '%Statement)])
      (define str (string->stmt stmt))
      (displayln str out)
      stmt))
  (displayln "" out)
  (displayln "(go!" out)
  (define exp
    (for/list ([exp (hash-ref (bld-world-tbl ws) '%Expected)])
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
             [(next!) (append (list #f λnext!) (cdr stmt))]
             [else (error (format "~a: ~a not a valid form." (car stmt) (cdr stmt)))])))
  ;; Initialize command tries for pending commands.
  (initialize-cmd-tries)
  ;; Populate the exp for validation.  
  (current-expected (hash-ref (bld-world-tbl ws) '%Expected))
  ;; Open the log file.
  (current-log (open-output-file log-file #:exists 'replace)))

(define (validate-puzzle ws state kval)
  (debug-printf "validate-puzzle ~a ~a~%" state kval)
  (define tbl (bld-world-tbl ws))
  (debug-printf "tbl=~a~%" tbl)
  (define cats (hash-ref tbl '%Category))
  (debug-printf "cats=~a~%" cats)
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
  (hash-set! tbl '%Category (map car inits))
  (for ([init inits])
    (hash-set! tbl (car init) (cdr init))))

(define (load-exp tbl exp)
  (hash-set! tbl '%Expected exp))

(define (load-stmts tbl stmts)
  (hash-set! tbl '%Statement stmts))

(define (read-file ws state kval)
  (debug-printf "read-file ~a ~a~%" state kval)
  (define tbl (bld-world-tbl ws))
  (with-handlers ([exn:fail:filesystem:errno?
                   (λ (e) (printf "~a~%Loading bypassed.~%" (exn-message e)))])
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
   (main
    (edit EVT-RIGHT       push-key         main edit)
    (edit EVT-STMT        beg-stmt         stmt input)
    (edit EVT-EXP        beg-exp           exp input)
    (edit EVT-ESC         #F               stmt input)
    (edit EVT-RUN         beg-solve        solve edit)
    (edit EVT-LOAD        read-file        main edit)
    (edit string->number  sel-key#         main edit)

    (error EVT-ESC         clear-key-flds! main edit))

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
    (edit EVT-RIGHT       push-key           cat edit)
    (edit EVT-LEFT        pop-key            cat edit)
    (edit EVT-SEL-VAL     sel-key-val        cat edit)
    (edit EVT-SEL-NUM     sel-key-num        cat edit)
    (edit EVT-STMT        beg-stmt           stmt input)
    (edit EVT-EXP        beg-exp           exp input)
    (edit string->number  sel-key#           cat edit))
   
   (stmt
    (input EVT-DEL        rem-stmt-tmp       stmt input)
    (input EVT-ENTER       add-stmt-val      stmt edit)
    (input EVT-MAIN        beg-main         main edit)
    (input EVT-EXP         beg-exp         exp input)
    (input EVT-ESC          clear-stmt-flds! stmt edit)

    (edit EVT-ADD          clear-stmt-flds!  stmt input)
    (edit EVT-DEL          rem-stmt-val      stmt edit)
    (edit EVT-UP           up-stmt-val       stmt edit)
    (edit EVT-DOWN         down-stmt-val     stmt edit)
    (edit EVT-MAIN       beg-main            main edit)
    (edit EVT-EXP        beg-exp             exp input)
    (edit EVT-ESC          clear-stmt-flds!  stmt edit)
    (edit EVT-LEFT         toggle-exp?      stmt edit)
    (edit EVT-RIGHT        toggle-exp?      stmt edit)
    (edit string->number   sel-stmt#         stmt edit)

    (error EVT-ESC         clear-stmt-flds!  stmt input))

   (exp
    (input EVT-DEL         rem-exp-tmp       exp input)
    (input EVT-ENTER       add-exp-val       exp edit)
    (input EVT-MAIN        beg-main          main edit)
    (input EVT-STMT        beg-stmt          stmt edit)
    (input EVT-ESC         clear-exp-flds!  exp edit)

    (edit EVT-ADD          clear-exp-flds!  exp input)
    (edit EVT-DEL          rem-exp-val      exp edit)
    (edit EVT-UP           up-exp-val       exp edit)
    (edit EVT-DOWN         down-exp-val     exp edit)
    (edit EVT-MAIN         beg-main          main edit)
    (edit EVT-STMT         beg-stmt          stmt edit)
    (edit EVT-ESC          clear-exp-flds!  exp edit)
    (edit EVT-LEFT         toggle-exp?      exp edit)
    (edit EVT-RIGHT        toggle-exp?      exp edit)
    (edit string->number   sel-exp#         exp edit)

    (error EVT-ESC         clear-exp-flds!  exp edit))

   (solve    
    (edit EVT-CMD          process-cmd       solve edit)
    (edit EVT-MAIN         beg-main          main edit)

    (msg  EVT-ESC         clear-solve-flds!  solve edit))))

;; Translate numpad values into equivalent non-numpad values. 
