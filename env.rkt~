#lang plai

;; Env is Symbol -> Value
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
