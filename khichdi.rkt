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
    [funV (param body env) (error 'interp/khichdi "bad number: ~a" v)]
    [boxV (l) (error 'interp/khichdi "bad number: ~a" l)]))


;; Value -> Boolean
;; produces true if the given value represents the number 0
(define (zero-value? v)
  (type-case Value v
    [numV (n) (= n 0)]
    [funV (param body env) #f]
    [boxV (l) #f]))


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
    [funV (x body env) (let ([l (gensym)])
                         (let-env/eff ([env])
                                      (let-state/eff ([store])
                                                     (with-env/eff (extend-env env x l)
                                                       (let/eff ([_ (update-state/eff
                                                                     (update-store store l v2))])
                                                                (interp/khichdi-eff body))))))]
    [boxV (l) (error 'interp/khichdi "bad function: ~a" v1)]))


;; KID Khichdi -> Computation
;; interpret the given fix expression
(define (interp-fix x body)
  (interp/khichdi-eff (subst body x (fix x body))))


;; Computation KID Khichdi Tag KID Khichdi -> Computation
;; interpret the given match/handle operation
;; Effect: signal an error in case of runtime error
(define-syntax interp/match-handle
  (syntax-rules ()
    [(_ cexpr val-id val-body etag eid ebody)
     (let-env/eff ([env])
                  (match-let ([`(,val ,store) (cexpr env empty-store)])
                    (type-case Canonical val
                      [value (v) 
                             (with-env/eff (extend-env env val-id v)
                               (interp/khichdi-eff val-body))]
                      [razed (tag payload) (if (symbol=? tag etag)
                                               (with-env/eff (extend-env env eid payload)
                                                 (interp/khichdi-eff ebody))
                                               (raise/eff tag payload))])))]))


;; Tag Computation  -> Computation
;; interpret the given raze operation
(define-syntax interp/raze
  (syntax-rules ()
    [(_ tag cexpr)
     (let/eff ([v cexpr])
              (raise/eff tag v))]))

;; Value -> (boxV _)
;; coerce the given value to a box location
;; Effect: signal an error if v does not denote a box location
(define (value->loc v)
  (match v
    [(boxV l) l]
    [else (error 'interp "Bad box location: ~a" v)]))

;; Value -> Computation
;; interpret the given newbox expression
(define (newbox-value v)
  (let ([loc (gensym)])
    (let-state/eff ([store])
                   (let/eff ([_ (update-state/eff (update-store store loc v))])
                            (return/eff (boxV loc))))))

;; Value Value -> Computation
;; interpret the given setbox expression
(define (setbox-value v1 v2)
  (let ([loc (value->loc v1)])
    (let-state/eff ([store])
                   (let/eff ([_ (update-state/eff (update-store store loc v2))])
                            (return/eff (boxV loc))))))

;; Value -> Computation
;; get back the value stored in the given box
;; EFFECTS: throws an error if not a box value
(define (openbox-value v)
  (let ([loc (value->loc v)])
    (let-state/eff ([store])
                   (return/eff (lookup-store store loc)))))

;; assign variable x to the value v
(define (setvar-value x v env store)
  (let* ([l (lookup-env env x)]
         [v-old (lookup-store store l)])    
    (list v-old 
          (update-store store l v))))


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
                         (let-state/eff ([store])
                                        (return/eff (lookup-store store
                                                                  (lookup-env env x)))))]
    [fun (x body) (let-env/eff ([env])
                               (return/eff (funV x body env)))]
    [app (rator rand) (let/eff* ([v1 (interp/khichdi-eff rator)]
                                 [v2 (interp/khichdi-eff rand)])
                                (apply-value v1 v2))] 
    [if0 (p c a)
         (let/eff ([pv (interp/khichdi-eff p)])
                  (if (zero-value? pv)
                      (interp/khichdi-eff c)
                      (interp/khichdi-eff a)))]
    [fix (x body) (interp-fix x body)]
    [match/handle (expr vid vbody etag eid ebody)
                  (interp/match-handle (interp/khichdi-eff expr)
                                       vid
                                       vbody
                                       etag
                                       eid
                                       ebody)]
    [raze (tag expr) (interp/raze tag (interp/khichdi-eff expr))]
    [newbox (e) (let/eff ([v (interp/khichdi-eff e)])
                         (newbox-value v))]
    [setbox (e1 e2) (let/eff* ([v1 (interp/khichdi-eff e1)]
                               [v2 (interp/khichdi-eff e2)])
                              (setbox-value v1 v2))]
    [openbox (e) (let/eff ([v (interp/khichdi-eff e)])
                          (openbox-value v))]
    [setvar (x e) (let-env/eff ([env])
                               (let-state/eff ([store])
                                              (let/eff* ([v (interp/khichdi-eff e)]
                                                         [_ (update-state/eff
                                                             (update-store store
                                                                           (lookup-env env x)
                                                                           v))])
                                                        (return/eff (numV -99)))))]))
                                                  


;; Khichdi -> Value
;; produce the result of interpreting the given Khichdi expression
;; EFFECT: signals an error in case of a runtime error
(define (interp/khichdi r)
  (run/eff (interp/khichdi-eff r) empty-env empty-store))

(test (interp/khichdi (num 2)) (numV 2))
(test (interp/khichdi (add (num 2) (num 3))) (numV 5))
(test (interp/khichdi (app (fun 'x (add (id 'x) (num 3))) (num 2)))
      (numV 5))
(test (interp/khichdi (app (fun 'x (app (fun 'y (add (id 'x) (id 'y))) (num 4))) (num 3)))
      (numV 7))
(test (interp/khichdi (app (fun 'x (if0 (id 'x) (num 1) (add (num 2) (id 'x)))) (num 0)))
      (numV 1))
(test (interp/khichdi (app (fun 'f (app (id 'f) (num 1)))
                           (fun 'x (add (id 'x) (num 1)))))
      (numV 2))
(test (interp/khichdi (app (fun 'f (app (id 'f) (num 1)))
                           (fun 'x (if0 (id 'x)
                                        (num 9)
                                        (add (id 'x) (num 2))))))
      (numV 3))
;; should diverge
;; (test (interp/khichdi (fix 'f (id 'f))) (numV 0))
(test (interp/khichdi (app (fun 'down (app (id 'down) (num 1)))
                           (fix 'f (fun 'x (if0 (id 'x)
                                                (num 9)
                                                (app (id 'f) (sub (id 'x) (num 1)))))))) (numV 9))
(test/exn (interp/khichdi (raze 'some-tag (num 2))) "uncaught exception:")
(test (interp/khichdi (match/handle (raze 'some-tag (num 2))
                                    'x
                                    (add (id 'x) (num 3))
                                    'some-tag
                                    'x
                                    (add (id 'x) (num 4))))
      (numV 6))

;; (test (interp/khichdi (newbox (num 2))) (boxV 'g2405468))
(test (interp/khichdi (with 'x (newbox (num 2)) (add (num 1) (openbox (id 'x))))) (numV 3))
(test (interp/khichdi (with 'x (newbox (num 3)) (seqn (setbox (id 'x) (num 4))
                                                      (add (num 1) (openbox (id 'x))))))
      (numV 5))
