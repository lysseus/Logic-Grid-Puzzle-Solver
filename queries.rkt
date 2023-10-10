#lang racket

;;;
;;; QUERIES
;;;

(provide (all-defined-out))

(require utils/list
         "common.rkt")

;; Returns the puzzle box value.
;; A puzzle hash entry consists of either a symbol or a set
;; containing a pair of keys (category and property).
;; The keypair search produces a "box" value (the intersection of
;; row and column) of a single puzzle grid.)
(define/contract (? key1 (key2 #f))
  (->* ((or/c category/c key/c)) ((or/c #f property/c key/c)) any)
  (cond
    [(and (pair? key1) (pair? key2))
     (hash-ref puzzle (set key1 key2))]
    [(and (pair? key1) (symbol? key2))
     (for/list ([prop2 (hash-ref puzzle key2)])
       (hash-ref puzzle (set key1 (cons key2 prop2))))]
    [(and (symbol? key1) (pair? key2))
     (for/list ([prop1 (hash-ref puzzle key1)])
       (hash-ref puzzle (set key2 (cons key1 prop1))))]
    [(and (symbol? key1) (false? key2))
     (hash-ref puzzle key1)]
    [else
     (error (format "Invalid keys for ? ~a ~a" key1 key2))]))

;; Returns a list of keyss matching a key (category and property)
;; for given box value. 
(define/contract (?= key (box-val #t))
  (->* (key/c) ((or/c boolean? category/c)) any)
  (define puzzle-keys (hash-keys puzzle))
  (define ls (map set->list
                  (filter (λ (pk) (and (set? pk)
                                       (set-member? pk key)))
                          puzzle-keys)))
  (map list->set (filter (λ (v) (eq? (? (first v) (second v)) box-val)) ls)))

;; Remove key from ?= key box-val
(define/contract (?-= key (box-val #t))
  (-> key/c boolean? any)
  (remove* (list key)
           (apply append (map set->list (?= key box-val)))))

;; Returns a list of properties along category for key that are not false.
;; These positions may eiher be true or unknown. 
(define (?prop key category)
  (reverse (for/fold ([acc empty])
                     ([v (? key category)])
             (if (false? v) acc (cons v acc)))))

;; Returns the property for the category for which the pair
;; key (cons category property) is true in the puzzle.
(define/contract (?true key category)
  (-> key/c category/c any)
  (define ans (assoc category (?-= key #t)))
  (if (false? ans) #f (cdr ans)))

;; Returns the remaining keys for the categories specified by keys,
;; removing the keys of the argument. 
;; For instance: (?~ '(x . 1) '(y . 2)) will return the list of all keys for categories
;; x and y with '(x . 1) and '(y . 2) removed from the list. 
(define (?~ . keys)
  (define assc (pairs->assoc keys))
  (assoc->pairs
   (for/list ([dvs assc])
     (define d-name (car dvs))
     (cons d-name
           (remove* (rest dvs) (? d-name))))))

;; Returns a matrix of puzzle box values corresponding to
;; the cat1 properties x cat2 properties. This is a values
;; representation of the cat1/cat2 subgrid of the puzzle. 
(define (?xcat cat1 cat2)
  (for/list ([prop1 (? cat1)])
    (for/list ([prop2 (? cat2)])
      (? (cons cat1 prop1) (cons cat2 prop2)))))

;; Returns a list of indexes along category for key that are not false.
;; These positions may eiher be true or unknown. 
(define (?n key category)
  (reverse (for/fold ([acc empty])
                     ([v (? key category)]
                      [n (in-naturals)])
             (if (false? v) acc (cons n acc)))))

;; Returns a list of indexes along category for key that are unknown.
(define (?unknown key category)
  (reverse (for/fold ([acc empty])
                     ([v (? key category)]
                      [n (in-naturals)])
             (if (unknown? v) (cons n acc) acc))))
