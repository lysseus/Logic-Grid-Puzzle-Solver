#lang racket

;;;
;;; STATEMENTS
;;;

(provide positive!      λpositive!
         negative!    λnegative!
         either-or!   λeither-or!
         either-and!  λeither-and!         
         seq!         λseq!
         next-to!     λnext-to!)

(require (for-syntax syntax/parse)
         utils/cmd-queue
         (only-in utils/list assoc->pairs pairs->assoc list->list*)
         (only-in "common.rkt" UNKNOWN true? unknown? 1? puzzle key/c
                  category/c valid-key/c suppress-error debug-printf
                  current-log)
         "clauses.rkt"
         (only-in "queries.rkt" ? ?= ?-= ?n ?unknown))

(define prev-stmt# (make-parameter #f))
(define (log . vs)
  (define stmt# (cmd-stmt# processing-cmd))
  (define str (string-append
               (cond
                 [(and (not (false? prev-stmt#))
                       (eq? stmt# (prev-stmt#)))
                  (format "    ~a " #\u25aa)]
                 [else (format "[~a] " stmt#)])
               (apply format vs)))  
  (if (false? (current-log))
      ;; Print the string to default output.
      (printf "~a~%" str)
      (displayln str (current-log)))
  (prev-stmt# stmt#))

;;;
;;; POSITIVE!
;;;

(define-syntax (positive! stx)
  (syntax-parse stx
    [(_ key1:expr key2:expr)
     #'(add-to-pending 1 λpositive!
                       (key-clause key1) (key-clause key2))]))

(define/contract (λpositve! key1 key2)
  (-> valid-key/c valid-key/c any)
  (log "positive! ~a ~a~%" key1 key2)
  (λrelate! #t key1 key2))

(define/contract λpositive!
  (case->
   (-> valid-key/c valid-key/c any)
   (-> boolean? valid-key/c valid-key/c any))

  ;; The case-lambda allows for two syntaxes: one with boolean and one without.
  ;; We simply add the default #t boolean to the arguments and re-route to that form
  ;; of the statement.
  (case-lambda
    [(key1 key2) (λrelate! #t key1 key2)]
    [(val key1 key2)
     (log "relate! ~a ~a ~a~%" val key1 key2)

     ;; Check if val is different from a previously set (non-unknown) value
     ; of the puzzle-box. If different this indciates an inconsistancy in the
     ;; puzzle clues, which are considered true and whose resolution cannot
     ;; contradict one another. 
     (define puzzle-box (? key1 key2))
     (cond
       [(unknown? puzzle-box) (void)]
       [(eq? puzzle-box val) (void)]
       [else (error (format "puzzle-box value ~a differs from val for ~a ~a ~a~%"
                            puzzle-box key1 key2 val))])

     ;; Check that we have not already marked true along
     ; the category/key row or column. If we have this means
     ;; there is either a problem in the code or in the
     ;; formulation of puzzle clues.
     (when (true? val)
       (check-for-trues key1 key2))
  
     ;; At this point we can set the puzzle-box value, if unknown. 
     (when (unknown? puzzle-box)
       (hash-set! puzzle (set key1 key2) val))

     ;; The following logic resolves relationships between
     ;; category1 and category2.
     (cond
       ;; when grid value is true we mark the othr row and column
       ;; values false. They cannot be true and are eliminated.
       [(true? val)
        (define cat1 (car key1))
        (debug-printf "cat1=~a~%" cat1)
        (for ([prop1 (? cat1)])
          (define key1 (cons cat1 prop1))
          (debug-printf "key1=~a~%" key1)
          (when (unknown? (? key1 key2))
            (λrelate! #f key1 key2)))
        (define cat2 (car key2))
        (debug-printf "cat2=~a~%" cat2)
        (for ([prop2 (? cat2)])
          (define key2 (cons cat2 prop2))
          (debug-printf "key2=~a~%" key2)
          (when (unknown? (? key1 key2))
            (λrelate! #f key1 key2)))]

       ;; val is false.
       [else 
        ;; When only 1 grid value in a row or column is unknown,
        ;; it must be true.
        (define (check-assign-true key1 key2)
          (define cat2 (car key2))
          (define result (? key1 cat2))
          (cond
            [(1? (count true? result)) (void)]
            [(1? (count unknown? result))
             (define prop2
               (list-ref (? cat2) (vector-member UNKNOWN
                                                 (list->vector result))))
             (λrelate! #t key1 (cons cat2 prop2))]
            [else (void)]))
        (check-assign-true key1 key2)
        (check-assign-true key2 key1)])

     ;; Fundamental Category Relationships
     ;; (c1 * c2) * (c1 * c3) = (c1 * c2) * (c2 * c3)

     ;; When (c1 * c2) is true and (c1 * c3) is true, (c2 * c3) is true.
     (when (true? val)
       (define (true+true->true key)
         (debug-printf "true+true->true ~a~%" key)
         (define trues (?-= key #t))
         (unless (empty? trues)
           (for/fold ([key1 (car trues)])
                     ([key2 (rest trues)])
             (debug-printf "key1=~a key2=~a~%" key1 key2)
             ;; If the categories are different and their box-val unknown
             ;; set their relationship to true.
             (when (and (not (eq? (car key1) (car key2)))
                        (unknown? (? key1 key2)))
               (λrelate! #t key1 key2))
             key2)))
       (true+true->true key1)
       (true+true->true key2))
     
     ;; wWen (c1 * c2) is true and (c1 * c3) is false, (c2 c3) is false.
     (define (true+false->false key)
       (define trues (?-= key #t))
       (unless (empty? trues)
         (for ([key1 trues])
           (define falses
             (filter-not (λ (v) (eq? (car v) (car key1)))
                         (?-= key #f)))        
           (for ([key2 falses])
             (when (unknown? (? key1 key2))
               (λrelate! #f key1 key2))))))  
     (true+false->false key1)
     (true+false->false key2)]))

;;;
;;; RELATE!
;;;

(define-syntax (relate! stx)
  (syntax-parse stx
    [(_ key1:expr key2:expr)
     #'(add-to-pending 1 λrelate!
                       (key-clause key1) (key-clause key2))]
    [(_ val:boolean key1:expr key2:expr)
     #'(add-to-pending 1 λrelate!
                       val (key-clause key1) (key-clause key2))]))

(define/contract λrelate!
  (case->
   (-> valid-key/c valid-key/c any)
   (-> boolean? valid-key/c valid-key/c any))

  ;; The case-lambda allows for two syntaxes: one with boolean and one without.
  ;; We simply add the default #t boolean to the arguments and re-route to that form
  ;; of the statement.
  (case-lambda
    [(key1 key2) (λrelate! #t key1 key2)]
    [(val key1 key2)
     (log "relate! ~a ~a ~a~%" val key1 key2)

     ;; Check if val is different from a previously set (non-unknown) value
     ; of the puzzle-box. If different this indciates an inconsistancy in the
     ;; puzzle clues, which are considered true and whose resolution cannot
     ;; contradict one another. 
     (define puzzle-box (? key1 key2))
     (cond
       [(unknown? puzzle-box) (void)]
       [(eq? puzzle-box val) (void)]
       [else (error (format "puzzle-box value ~a differs from val for ~a ~a ~a~%"
                            puzzle-box key1 key2 val))])

     ;; Check that we have not already marked true along
     ; the category/key row or column. If we have this means
     ;; there is either a problem in the code or in the
     ;; formulation of puzzle clues.
     (when (true? val)
       (check-for-trues key1 key2))
  
     ;; At this point we can set the puzzle-box value, if unknown. 
     (when (unknown? puzzle-box)
       (hash-set! puzzle (set key1 key2) val))

     ;; The following logic resolves relationships between
     ;; category1 and category2.
     (cond
       ;; when grid value is true we mark the othr row and column
       ;; values false. They cannot be true and are eliminated.
       [(true? val)
        (define cat1 (car key1))
        (debug-printf "cat1=~a~%" cat1)
        (for ([prop1 (? cat1)])
          (define key1 (cons cat1 prop1))
          (debug-printf "key1=~a~%" key1)
          (when (unknown? (? key1 key2))
            (λrelate! #f key1 key2)))
        (define cat2 (car key2))
        (debug-printf "cat2=~a~%" cat2)
        (for ([prop2 (? cat2)])
          (define key2 (cons cat2 prop2))
          (debug-printf "key2=~a~%" key2)
          (when (unknown? (? key1 key2))
            (λrelate! #f key1 key2)))]

       ;; val is false.
       [else 
        ;; When only 1 grid value in a row or column is unknown,
        ;; it must be true.
        (define (check-assign-true key1 key2)
          (define cat2 (car key2))
          (define result (? key1 cat2))
          (cond
            [(1? (count true? result)) (void)]
            [(1? (count unknown? result))
             (define prop2
               (list-ref (? cat2) (vector-member UNKNOWN
                                                 (list->vector result))))
             (λrelate! #t key1 (cons cat2 prop2))]
            [else (void)]))
        (check-assign-true key1 key2)
        (check-assign-true key2 key1)])

     ;; Fundamental Category Relationships
     ;; (c1 * c2) * (c1 * c3) = (c1 * c2) * (c2 * c3)

     ;; When (c1 * c2) is true and (c1 * c3) is true, (c2 * c3) is true.
     (when (true? val)
       (define (true+true->true key)
         (debug-printf "true+true->true ~a~%" key)
         (define trues (?-= key #t))
         (unless (empty? trues)
           (for/fold ([key1 (car trues)])
                     ([key2 (rest trues)])
             (debug-printf "key1=~a key2=~a~%" key1 key2)
             ;; If the categories are different and their box-val unknown
             ;; set their relationship to true.
             (when (and (not (eq? (car key1) (car key2)))
                        (unknown? (? key1 key2)))
               (λrelate! #t key1 key2))
             key2)))
       (true+true->true key1)
       (true+true->true key2))
     
     ;; wWen (c1 * c2) is true and (c1 * c3) is false, (c2 c3) is false.
     (define (true+false->false key)
       (define trues (?-= key #t))
       (unless (empty? trues)
         (for ([key1 trues])
           (define falses
             (filter-not (λ (v) (eq? (car v) (car key1)))
                         (?-= key #f)))        
           (for ([key2 falses])
             (when (unknown? (? key1 key2))
               (λrelate! #f key1 key2))))))  
     (true+false->false key1)
     (true+false->false key2)]))

(define (check-for-trues key1 key2)
  (debug-printf "check-for-trues ~a ~a~%" key1 key2)
  ;; check if key1 for this grid already contains true.
  (define ans1 (remove (set key1 key2) (?= key1 #t)))
  (debug-printf "ans1=~a~%" ans1)
  (define trues1 (map set->list ans1))
  (debug-printf "trues1 ~a=~a~%" key1 trues1)
  (define vs1 (filter (λ (v) (and (member key1 v)
                                  (eq? (car key2)
                                       (caar (remove key1 v))))) trues1))
  (debug-printf "vs1=~a~%" vs1)
  (unless (empty? vs1)
    (error (format "λrelate! ~a ~a already trrue for ~a~%"
                   key1 key2 vs1)))
  ;; Check if key2 for this grid already contains true.
  (define ans2 (remove (set key1 key2) (?= key2 #t)))
  (define trues2 (map set->list ans2))
  (debug-printf "trues2 ~a=~a~%" key2 trues2)
  (define vs2 (filter (λ (v) (and (member key2 v)
                                  (eq? (car key1)
                                       (caar (remove key2 v))))) trues2))
  (debug-printf "vs2=~a~%" vs2)
  (unless (empty? vs2)
    (error (format "λrelate! ~a ~a already trrue for ~a~%"
                   key1 key2 vs2))))

;;;
;;; negative!
;;;;

(define-syntax (negative! stx)
  (syntax-parse stx
    [(_ key ...)
     #'(add-to-pending 1 λnegative! (key-clause key) ...)]))

(define (λnegative! . keys)
  (log "negative! ~a~%" keys)
  (define (assign-false keys)
    (for* ([key1 keys]
           [key2 (remove key1 keys)])
      (unless (eq? (car key1) (car key2))
        (λrelate! #f key1 key2))))
  (assign-false keys))

;;;
;;; either-or
;;;

;; either-or!: Compares the puzzle value at s1 with the puzzle
;; value at s2.
(define-syntax (either-or! stx)
  (syntax-parse stx
    #:datum-literals (:)
    [(_ (category:id : property:expr) [key1 key2])
     #'(add-to-pending #f λeither-or!          
                       (list (cons (quote category)
                                   (category-apply (quote category) property))
                             (key-clause key1))
                       (list (cons (quote category)
                                   (category-apply (quote category) property))
                             (key-clause key2)))]    
    [(_ key-pair1 key-pair2)
     #'(add-to-pending #f λeither-or!
                       (key-pair-clause key-pair1) (key-pair-clause key-pair2))]))
(define (λeither-or! key1 key2)
  (log "either-or! key1=~a key2=~a~%" key1 key2)
  ;; Both prop-names are the same. Set the remaining prop-vals to false.
  (cond
    [(and (eq? (caar key1) (caar key2))
          (eq? (caadr key1) (caadr key2)))
     (debug-printf "prop-names equal for ~a ~a~%" key1 key2)
     (define assc (map remove-duplicates (pairs->assoc (append key1 key2))))
     (debug-printf "assc=~a~%" assc)    
     (define ones (filter (λ (v) (= (length v) 2)) assc))
     (debug-printf "ones=~a~%" ones)
     (when (1? (length ones))
       (define prop1 (list->list* (car ones)))
       (define rems (remove (car ones) assc))
       (debug-printf "rems=~a~%" rems)
       (define rems2 (for/list ([vs rems])
                       (cons (car vs) (remove* (cdr vs) (? (car vs))))))
       (debug-printf "rems2=~a~%" rems2)
       (define keys (assoc->pairs rems2))
       (define prop2s (assoc->pairs rems2))
       (debug-printf "prop1=~a prop2s=~a~%" prop1 prop2s)
       (for ([prop2 prop2s])
         (λrelate! #f prop1 prop2)))]
    ;; The 2nd keys of each proposition are distinct.
    [(eq? (caar key1) (caar key2))
     (λrelate! #f (cadr key1) (cadr key2))]
    ;; The 1st keys of each proposition are distinct.
    [(eq? (caadr key1) (caadr key2))
     (λrelate! #f (car key1) (car key2))]
    ;; Boh keys of the propositions are distinct.
    [else
     (λrelate! #f (car key1) (car key2))
     (λrelate! #f (cadr key1) (cadr key2))])
  (define ans1 (? (car key1) (cadr key1)))
  (define ans2 (? (car key2) (cadr key2)))
  (debug-printf "ans1=~a ans2=~a~%" ans1 ans2)
  (cond
    ;; Both are true, we have an error.
    [(and (eq? #t ans1) (eq? #t ans2))
     (raise-user-error
      (format "λeither-or! ~a ~a both propositons are true." key1 key2))]
    ;; 1 is true, we don't need to do anything.
    [(or (eq? #t ans1) (eq? #t ans2)) (void)]        
    ;; Both are false, we have an error.
    [(and (false? ans1) (false? ans2))
     (raise-user-error
      (format "λeither-or! ~a ~a both propositons are false." key1 key2))]
    ;; Both are unknown, unable to deduce at this time.
    [(and (eq? '? ans1) (eq? '? ans2))
     (raise-user-error
      (format "λeither-or! ~a ~a both propositons are unknown." key1 key2))]
    ;; The 1st proposition is false, 2nd must be true.
    [(false? ans1)
     (λrelate! #t (car key2) (cadr key2))]
    [else (λrelate! #t (car key1) (cadr key1))]))

;;;
;;; either-and!
;;;

(define-syntax (either-and! stx)
  (syntax-parse stx
    [(_ [k1 k2] [k3 k4])
     #'(add-to-pending #f λeither-and!
                       (key-clause k1)
                       (key-clause k2)
                       (key-clause k3)
                       (key-clause k4))]))

(define (λeither-and! key1 key2 key3 key4)
  (log "criss-coross! ~a ~a ~a ~a~%"
       key1 key2 key3 key4)
  ;; Set negative relationshiops between 1st pair of props.
  (apply λnegative! (list key1 key2))
  
  ;; Set negative relationships between 2nd pair of props.
  (apply λnegative! (list key3 key4))

  (define cat1 (car key1))
  (define prop1 (cdr key1))
  (define cat2 (car key2))  
  (define prop2 (cdr key2))

  (define cat3 (car key3))
  (define prop3 (cdr key3))
  (define cat4 (car key4))
  (define prop4 (cdr key4))
  
  ;; name c1 = c2 => negate rems for c3 and c4
  (when (eq? cat1 cat2)
    (for ([prop1 (remove* (list prop1 prop2) (? cat1))])
      (define prop (cons cat1 prop1))
      (λrelate! #f key3 prop)
      (λrelate! #f key4 prop)))

  ;; name c3 = c4 => negate rems for c1 and c2
  (when (eq? cat3 cat4)
    (for ([prop3 (remove* (list prop3 prop4) (? cat3))])
      (define prop (cons cat3 prop3))
      (λrelate! #f key1 prop)
      (λrelate! #f key2 prop)))
  
  ;; name c1 = c3 => negate rems for c2 and c4
  (when (eq? cat1 cat3)
    (for ([prop1 (remove* (list prop1 prop3) (? cat1))])
      (define prop (cons cat1 prop1))
      (λrelate! #f key2 prop)
      (λrelate! #f key4 prop)))
  
  ;; name c2 = c4 => negate rems for c1 and c3
  (when (eq? cat2 cat4)
    (for ([prop2 (remove* (list prop2 prop4) (? cat2))])
      (define prop (cons cat2 prop2))
      (λrelate! #f key1 prop)
      (λrelate! #f key3 prop)))
    
  ;; name c1 = c4 => negate rems for c2 and c3
  (when (eq? cat1 cat4)
    (for ([prop1 (remove* (list prop1 prop4) (? cat1))])
      (define prop (cons cat1 prop1))
      (λrelate! #f key2 prop)
      (λrelate! #f key3 prop)))
      
  ;; name c2 = c3 => negate rems for c1 and c4
  (when (eq? cat2 cat3)
    (for ([prop3 (remove* (list prop2 prop3) (? cat2))])
      (define prop (cons cat2 prop3))
      (λrelate! #f key1 prop)
      (λrelate! #f key4 prop)))
  
  (when (false? (? key1 key3))
    (debug-printf "false[1] ~a ~a~%" key1 key3)
    (λrelate! #t key1 key4)
    (λrelate! #t key2 key3))
  
  (when (false? (? key1 key4))
    (debug-printf "false[2] ~a ~a~%" key1 key4)
    (λrelate! #t key1 key3)
    (λrelate! #t key2 key4))
  
  (when (false? (? key2 key3))
    (debug-printf "false[3] ~a ~a~%" key2 key3)
    (λrelate! #t key2 key4)
    (λrelate! #t key1 key3))
  
  (when (false? (? key2 key4))
    (debug-printf "false[4] ~a ~a~%" key2 key4)
    (λrelate! #t key2 key3)
    (λrelate! #t key1 key4))
    
  (define ans1
    (suppress-error (λeither-or! (list key1 key3)
                           (list key1 key4))))
  (define ans2
    (suppress-error (λeither-or! (list key2 key3)
                           (list key2 key4))))
  
  (unless (and ans1 ans2)
    (raise-user-error "either-and ~a ~a ~a ~a unresolved.~%"
                      key1 key2 key3 key4)))

(define (check-seq-idxs mn mx group-results)
  (debug-printf "check-seq-idxs ~a ~a ~a~%" mn mx group-results)
  (define ans
    (cond
      [(= 1 mx mn)
       group-results]
      [(= 1 mn)
       (normalize-indexes group-results)]
      ;; We have multiple options, but only 1 possible sequence.
      [(and (= mn mx (length group-results))
            (apply equal? group-results))
       (map list (car group-results))]
      [else (raise-user-error (format "seq ~a has ~a solutions."
                                      group-results mx))]))
  (debug-printf "ans=~a~%" ans)
  ans)

(define (group d n)
  (define-values (acc rem)
    (for/fold ([acc empty][rem (range d)])
              ([cnt (range n)])      
      (values (cons (take rem (add1 (- d n))) acc)
              (rest rem))))
  (reverse acc))

(define (normalize-indexes idxs)
  (define (order-indexes idxs)
    (for/list ([idxs idxs]
               [n (in-naturals)])
      (list n idxs)))
  (define (ones-filter idxs)
    (filter (λ (v) (= 1 (length (second v))))
            idxs))
  (define (loop ordered-idxs)    
    (define ones (ones-filter ordered-idxs))
    (define many (filter-not (λ (v) (= 1 (length (second v))))
                             ordered-idxs))
    (define new-many
      (for/fold ([many many])
                ([o ones])
        (map (λ (m)
               (define many-pos (first m))
               (define many-idxs (second m))
               (define ones-pos (first o))
               (define ones-idxs (second o))
               (list many-pos
                     (filter (λ (many-idx)                                    
                               ((if (< ones-pos many-pos) < >)
                                (first ones-idxs) many-idx)) many-idxs)))
             many)))
    (sort ( append ones new-many) < #:key car))
  (define ordered-idxs
    (for/fold ([idxs (order-indexes idxs)])
              ([n (range (sub1 (length idxs)))])
      (loop idxs)))
  (if (= (length idxs) (length (ones-filter ordered-idxs)))
      (map second ordered-idxs)
      (raise-user-error
       (format "normalize-indexes ~a not normalize-indexesalized." idxs))))

;;;
;;; SEQ 
;;;

(define-syntax (seq! stx)
  (syntax-parse stx    
    [(_ category:id key:expr ...)
     #'(add-to-pending #f λseq! (quote category)
                       (key-clause key) ...)]
    [(_ key:expr ...)
     #'(add-to-pending #f λseq! (key-clause key) ...)]))

(define-syntax (next-to! stx)
  (syntax-parse stx    
    [(_ category:id key:expr ...)
     #'(add-to-pending #f λnext-to! (quote category)
                       (key-clause key) ...)]))

(define (λseq! . keys)
  (log "seq! ~a~%" keys)
  (cond
    [(symbol? (first keys)) (apply seq<! keys)]
    [else (apply shift! keys )]))

(define (seq<! category . keys)
  (debug-printf "seq ~a ~a~%" category keys)  
  (define properties (? category))
  (debug-printf "properties=~a~%" properties)
  ;; The keys are distinct from one another.
  (for* ([key1 keys]
         [key2 (remove key1 keys)])
    (unless (eq? (car key1) (car key2))
      (λrelate! #f key1 key2)))
  (define keys-results
    (for/list ([key1 keys])
      (for/list ([property properties])
        (? key1 (cons category property)))))
  (debug-printf "keys-results=~a~%" keys-results)
  (define keys-idxs
    (reverse (for/fold ([acc empty])
                       ([key-results keys-results])
               (cons (reverse (for/fold ([acc empty])
                                        ([key-result key-results]
                                         [n (in-naturals)])
                                (if (false? key-result)
                                    acc
                                    (cons n acc))))
                     acc))))
  (debug-printf "keys-idxs=~a~%" keys-idxs)
  (define-values (mn mx group-results)
    (build-group-idxs (length properties) keys-idxs))
  (debug-printf "mn=~a mx=~a group-results=~a~%" mn mx group-results)
  ;; We have at least 1 empty set. This statement as coded can never be true.
  ;; As such, it must be miscoded. 
  (when (zero? mn) (error (format "seq ~a has ~a solutions."
                                  keys-idxs mn)))
  ;; At this point we must assign all outlier values (those indexes
  ;; outside the groupings as false for each key sequenced.
  (define properties-idxs (range (length properties)))
  (for ([group-result group-results]
        [key1 keys])
    (define outliers (remove* group-result properties-idxs))
    (for ([key-idx outliers])
      (define property (list-ref properties key-idx))
      (define key2 (cons category property))
      (debug-printf "λrelate! ~a ~a false.~%" key1 key2)
      (λrelate! #f key1 key2)))
  ;; Validate and normalize the group results.
  (define seq-idxs (check-seq-idxs mn mx group-results))
  (debug-printf "seq-idxs=~a~%" seq-idxs)
  ;; If we return here we passed the check.
  ;; Assign the key sequence.
  (for ([idx seq-idxs]
        [key keys])
    (define property (list-ref properties (car idx)))
    (λrelate! #t key (cons category property))))

(define (build-group-idxs prop-vals-length keys-idxs)
  (define group-indexes (group prop-vals-length (length keys-idxs)))
  (debug-printf "group-indexes=~a~%" group-indexes)
  ;; Map the key reults to sequence groups.
  (define group-results
    (map reverse
         (for/list ([idxs keys-idxs]
                    [gidxs group-indexes])
           (set->list (set-intersect (list->set idxs)
                                     (list->set gidxs))))))
  (define mn (apply min (map length group-results)))
  (define mx (apply max (map length group-results)))
  (values mn mx group-results))

;; Establishes a relationship between 2 properties that are
;; n prop-vals apart within prop-name. The sign of n determins
;; the direction of the relationship between prop1 and prop2. Positive
;; means prop2 has a greater positional value along prop-name than prop1.
;; Negative means that the reverse is true. 
(define/contract (shift! category/n key1 key2)
  (-> (cons/c category/c integer?) key/c key/c any)
  (debug-printf "shift! ~a ~a~a~%"
                category/n key1 key2)
  (define category (car category/n))

  ;; Prop1 and prop2 have a negative relationship.
  (λnegative! key1 key2)
  
  (define-values (resolved? marks) (shift? category/n key1 key2))
  (cond
    [(false? resolved?)
     (define prop1s (first marks))
     (for ([property prop1s])
       (λrelate! #f key1 (cons category property)))
     (define prop2s (second marks))
     (for ([property prop2s])
       (λrelate! #f key2 (cons category property)))
     (raise-user-error "seq ~a ~a ~a unresolved." category/n key1 key2)]
    [else
     (define prop1 (first marks))
     (unless (false? prop1)
       (λrelate! #t key1 (cons category prop1)))
     (define prop2 (second marks))
     (unless (false? prop2)
       (λrelate! #t key2 (cons category prop2)))]))

(define (shift? key1 key2 key3)
  (debug-printf "?shift ~a ~a ~a~%" key1 key2 key3)
  (define cat1 (car key1))
  (define prop1 (cdr key1))
  (define prop1s (? cat1))
  (define prop1-idxs (range (length prop1s)))
  
  (define cat2 (car key2))
  (define prop2 (cdr key2))
  
  (define cat3 (car key3))
  (define prop3 (cdr key3))

  ;; not-false values for key2
  (define key2-not-false (?n key2 cat1))

  ;; not-false values for key3
  (define key3-not-false (?n key3 cat1))
  (debug-printf "key2-not-false=~a key3-not-false=~a~%" key2-not-false key3-not-false)

  (define prop2-false-idxs (remove* key2-not-false prop1-idxs))
  (define prop3-false-idxs (remove* key3-not-false prop1-idxs))
  (debug-printf "prop2-false-idxs=~a prop3-false-idxs=~a~%" prop2-false-idxs prop3-false-idxs)

  (define valid-prop2s
    (set->list
     (set-intersect (list->set key2-not-false)
                    (for/set ([n key3-not-false])
                      (- n prop1)))))

  (define valid-prop3s
    (set->list
     (set-intersect (list->set key3-not-false)
                    (for/set ([n key2-not-false])
                      (+ n prop1)))))
  
  (debug-printf "valid-prop2s=~a valid-prop3s=~a~%" valid-prop2s valid-prop3s)
  
  (define prop2-mark-false-idxs (remove* (append prop2-false-idxs valid-prop2s) prop1-idxs))
  (define prop3-mark-false-idxs (remove* (append prop3-false-idxs valid-prop3s) prop1-idxs))
  (debug-printf "prop2-mark-false-idxs=~a prop3-mark-false-idxs=~a~%"
                prop2-mark-false-idxs prop3-mark-false-idxs)
  (define prop2-mark-false-vals (map (λ (n) (list-ref prop1s n)) prop2-mark-false-idxs))
  (define prop3-mark-false-vals (map (λ (n) (list-ref prop1s n)) prop3-mark-false-idxs))
  (debug-printf "prop2-mark-false-vals=~a prop3-mark-false-vals=~a~%"
                prop2-mark-false-vals prop3-mark-false-vals)
   
  (cond
    [(and (= (length valid-prop2s) 1) (= (length valid-prop3s) 1))
     (define prop2-prop-val (list-ref prop1s (car valid-prop2s)))
     (define prop3-prop-val (list-ref prop1s (car valid-prop3s)))
     (debug-printf "prop2-prop-val=~a prop3-prop-val=~a~%" prop2-prop-val prop3-prop-val)
     
     (define prop2-mark-val
       (cond
         [(eq? #t (? key2 (cons cat1 prop2-prop-val))) #f]
         [else prop2-prop-val]))
     (define prop3-mark-val
       (cond
         [(eq? #t (? key3 (cons cat1 prop3-prop-val))) #f]
         [else prop3-prop-val]))
     (values #t (list prop2-mark-val prop3-mark-val))]
    [else (values #f (list prop2-mark-false-vals prop3-mark-false-vals))]))

(define (λnext-to! . keys)
  (log "next-to! ~a~%" keys)
  (apply either-or-shift! (cons (car keys) 1) (cdr keys)))

(define/contract (either-or-shift! category/n key1 key2)
  (-> key/c key/c key/c any)
  (debug-printf "either-or-shift! ~a ~a~a~%"
                category/n key1 key2)
  (define category (car category/n))
  (define props (? category))
  
  ;; Prop1 and prop2 have a negative relationship.
  (λnegative! key1 key2)

  ;; Get key1 unknowns
  (define key1-unknowns (map (λ (n) (list-ref props n))
                             (?unknown key1 category)))
  ;; Test shift for key1 key2
  (define-values (key1-resolved? key1-marks) (shift? category/n key1 key2))  
  (debug-printf "key1-unknowns=~a~%" key1-unknowns)
  (debug-printf "key1-resolved?=~a key1-marks=~a~%" key1-resolved? key1-marks)

  ;; Get key2 unknowns
  (define key2-unknowns
    (map (λ (n) (list-ref props n)) (?unknown key2 category)))
  ;; Test key2 key1 shift
  (define-values (key2-resolved? key2-marks) (shift? category/n key2 key1))  
  (debug-printf "key2-unknowns=~a~%" key2-unknowns)
  (debug-printf "key2-resolved?=~a key2-marks=~a~%" key2-resolved? key2-marks)
   
  (cond
    ;; Neither are resolved. Fail the statement.
    [(and (false? key1-resolved?) (false? key2-resolved?))
     (raise-user-error "next-to! ~a ~a ~a unresolved." category/n key1 key2)]

    ;; key1->key2 is resolved, key2->key1 is not. Do key1->key2 shift.
    [(false? key2-resolved?)
     (debug-printf "Resolved for key1->key2~%.")      
     (shift! category/n key1 key2)]
     
    ;; key2->key1 is resolved, but key1->key2 is not. Do the shift. 
    [(false? key1-resolved?)
     (debug-printf "Resolved for key2->kkey1.~%")
      
     (shift! category/n key2 key1)]
     
    ;; Both are resolved. Mark boxes to be excluded.
    [else
     (debug-printf "Resolved for key1->key2 and key2->key1.~%")
     (define key1-rems (remove* (append key1-marks key2-marks) key1-unknowns))
     (debug-printf "key1-rems=~a~%" key1-rems)
     (define key2-rems (remove* (append key1-marks key2-marks) key2-unknowns))
     (debug-printf "key2-rems=~a~%" key2-rems)
     ;; Mark key1 negative relationships.
     (for ([n key1-rems])
       (λrelate! #f key1 (cons category n)))
     ;; Mark key2 negative relationships.
     (for ([n key2-rems])
       (λrelate! #f key2 (cons category n)))
     (raise-user-error "next-to! ~a ~a ~a unresolved." category/n key1 key2)]))
