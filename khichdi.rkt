#lang plai

(require "prelude.rkt")   ;; Khichdi struct definitions
(require "env.rkt")       ;; env implementation
(require "abstract.rkt")  ;; effect abstraction interface

(print-only-errors #t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpretation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Value -> Number
;; produces the number associated to the given value
;; EFFECT: signals an error in case of a runtime error
(define (value->num v)
  (type-case Value v
    [numV (n) n]
    [funV (param body env) (error 'interp/khichdi "bad number: ~a" v)]))


;; Value -> Boolean
;; produces true if the given value represents the number 0
(define (zero-value? v)
  (type-case Value v
    [numV (n) (= n 0)]
    [funV (param body env) #f]))



;; (Number Number -> Number) Value Value  -> Value
;; apply num-op to the numbers represented by v1 and v2
;; Effect: signal an error if either argument does not represent a number
(define (num-binop-value num-op v1 v2)
  (let ([n1 (value->num v1)]
        [n2 (value->num v2)])
    (numV (num-op n1 n2))))


;; Value Value -> Computation
;; produces the sum of the two numbers represented by the given values
;; EFFECT: signals an error if values cannot be converted to numbers
(define (add-value v1 v2)
  (return/eff (num-binop-value + v1 v2)))


;; Value Value -> Computation
;; produces the difference of the two numbers represented by the given values
;; EFFECT: signals an error if values cannot be converted to numbers
(define (sub-value v1 v2)
  (return/eff (num-binop-value - v1 v2)))


;; Value Value -> Computation
;; produces the result of applying v1 to v2
;; EFFECT: signals an error if v1 is not a function value
(define (apply-value v1 v2)
  (type-case Value v1
    [numV (n) (error 'interp/khichdi "bad function: ~a" v1)]
    [funV (x body env) (let-env/eff ([env])
                                    (with-env/eff (extend-env env x v2)
                                      (interp/khichdi-eff body)))]))


;; Computation KID Khichdi Tag KID Khichdi -> Computation
;; interpret the given match/handle operation
;; Effect: signal an error in case of runtime error
(define-syntax interp/match-handle
  (syntax-rules ()
    [(_ cexpr val-id val-body etag eid ebody)
     (let-env/eff ([env])
                  (type-case Canonical (cexpr env)
                    [value (v) 
                           (with-env/eff (extend-env env val-id v)
                             (interp/khichdi-eff val-body))]
                    [razed (tag payload) (if (symbol=? tag etag)
                                             (with-env/eff (extend-env env eid payload)
                                               (interp/khichdi-eff ebody))
                                             (raise/eff tag payload))]))]))


;; Tag Computation  -> Computation
;; interpret the given raze operation
(define-syntax interp/raze
  (syntax-rules ()
    [(_ tag cexpr)
     (let/eff ([v cexpr])
              (raise/eff tag v))]))


;; Khichdi -> Computation
;; consumes a Khichdi and produces the correspoding Value
(define (interp/khichdi-eff r)
  (type-case Khichdi r
    [num (n) (return/eff (numV n))]
    [add (l r) (let/eff* ([v1 (interp/khichdi-eff l)]
                          [v2 (interp/khichdi-eff r)])
                         (add-value v1 v2))]
    [sub (l r) (let/eff* ([v1 (interp/khichdi-eff l)]
                          [v2 (interp/khichdi-eff r)])
                         (sub-value v1 v2))]
    [id (x) (let-env/eff ([env])
                         (return/eff (lookup-env env x)))]
    [fun (x body) (let-env/eff ([env])
                               (return/eff (funV x body env)))]
    [app (rator rand) (let/eff* ([v1 (interp/khichdi-eff rator)]
                                 [v2 (interp/khichdi-eff rand)])
                                (apply-value v1 v2))]
    [if0 (p c a)
         (let/eff* ([pv (interp/khichdi-eff p)]
                    [cv (interp/khichdi-eff c)]
                    [av (interp/khichdi-eff a)])
                   (if (zero-value? pv)
                       (return/eff cv)
                       (return/eff av)))]
    [match/handle (expr vid vbody etag eid ebody)
                  (interp/match-handle (interp/khichdi-eff expr)
                                       vid
                                       vbody
                                       etag
                                       eid
                                       ebody)]
    [raze (tag expr) (interp/raze tag (interp/khichdi-eff expr))]))


;; Khichdi -> Value
;; produce the result of interpreting the given Khichdi expression
;; EFFECT: signals an error in case of a runtime error
(define (interp/khichdi r)
  (run/eff (interp/khichdi-eff r) empty-env))

(test (interp/khichdi (num 2)) (numV 2))
(test (interp/khichdi (add (num 2) (num 3))) (numV 5))
(test (interp/khichdi (app (fun 'x (add (id 'x) (num 3))) (num 2)))
      (numV 5))
(test (interp/khichdi (app (fun 'x (app (fun 'y (add (id 'x) (id 'y))) (num 4))) (num 3)))
      (numV 7))
(test (interp/khichdi (app (fun 'x (if0 (id 'x) (num 1) (add (num 2) (id 'x)))) (num 0)))
      (numV 1))
(test/exn (interp/khichdi (raze 'some-tag (num 2))) "uncaught exception:")
(test (interp/khichdi (match/handle (raze 'some-tag (num 2))
                                    'x
                                    (add (id 'x) (num 3))
                                    'some-tag
                                    'x
                                    (add (id 'x) (num 4))))
      (numV 6))
