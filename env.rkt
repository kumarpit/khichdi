#lang plai
(require "prelude.rkt")

;; Env is Symbol -> Location
;; interp.  bindings of identifiers to objects of type X

;; empty-env : Env
;; Effect: signals an error if symbol is not there
(define empty-env
  (λ (x) (error 'lookup-env "Unbound symbol: ~a" x)))

;; extend-env : Env Symbol Value -> Env
(define (extend-env env x0 v)
  (λ (x)
    (if (symbol=? x x0)
        v
        (env x))))

(define (lookup-env env x) (env x))

;;
;; Stores
;;

;; Store is (listof (list Location Value))
;;

;; Store
;; an empty store (initial store)
(define empty-store empty)

;; Store 
(define (lookup-store store loc)
  (cond
    [(assoc loc store) => cadr] ;; consult cond docs if this looks crazy :)
    [else (error 'interp "Unbound Store Location: ~a" loc)]))


;; Store -> (list Location Store)
;; produce a new location in the given store
;; Effect: performs a gensym
(define (new-location store) (list (gensym) store))


;; Store Location Value -> Store
;; produce a new store that is updated with the new binding
(define (update-store store loc value)
  (cond
    [(empty? store) (list (list loc value))]
    [else ;; cond
     (if (symbol=? (first (first store)) loc)
         (cons (list loc value) (rest store))
         (cons (first store)
               (update-store (rest store) loc value)))]))


;; Any -> Boolean
;; produce true if the given value is a store, otherwise false
(define (Store? s)
  (and (list s)
       (for/and ([b s])
         (match s
           [(list loc value) (and (location? loc) (Value? value))]
           [else #f]))))

