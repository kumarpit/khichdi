#lang racket

(require "prelude.rkt") ;; Khichdi struct definitions
(provide (all-defined-out))

;; (let-env/eff ([env]) e) - bind f to the current accumulator and evalute e
;; (with-env/eff env e)    - run e with the accumulator set to env for its dynamic
;;                           extent
;; (raise/eff t v)         - return an exception with tag t and payload v
;; 
;; (let-state/eff ([state]) e) - bind state to the current accumulator and evalute e
;; (update-state/eff state)    - update state with new value


;; The Generic Interface Components:
;; (return/eff e)               - return the value of e
;; (run/eff e env0 state0)      - run computation e, starting with accumulator env0
;; (let/eff ([x e1] e2))        - bind x to the value of e1 in e2, threading factor
;; (let/eff* ([x* e1*] ...) e2) - sequentialize instances of let/eff

;;
;; Effect Implementation
;;


;; Computation is Env Store -> (list Canonical Store)
;; interp.  a function that awaits a standard accumulator and produces a (canonical) value

(define-syntax let-env/eff
  (syntax-rules ()
    [(_ ([env]) c)
     (λ (env state) (c env state))]))


;; Env Computation -> Computation
;; run c in a context where the environment is set to env
(define (with-env/eff env c)
  (λ (env0 state) (c env state)))

(define-syntax let-state/eff
  (syntax-rules ()
    [(_ ([state]) c)
     (λ (env state) (c env state))]))

;; Store -> Computation
;; sets the current state
(define (update-state/eff state)
  (λ (env state0)
    (list (value (numV -99)) state))) ; uninformative return value

 
;; Value -> Computation
;; create a computation that yields n
(define (return/eff v)
  (λ (env state) (list (value v) state)))


;; Tag Value -> Computation
(define (raise/eff t v)
  (λ (env state) (list (razed t v) state)))

;; Computation -> Value
;; run the given computation and produce its result
(define (run/eff c env0 state0)
  (match-let ([`(,val ,state) (c env0 state0)])
    (type-case Canonical val
      [value (v) v]
      [razed(tag payload) (error 'interp/khichdi "uncaught exception: ~a"
                                 (razed tag payload))])))

;; Compose two computations
(define-syntax let/eff
  (syntax-rules ()
    [(_ ([x c1]) c2)
     (λ (env state0)
       (match-let ([`(,val ,state) (c1 env state0)])
         (type-case Canonical val
           [value (v) (let ([x v]) (c2 env state))]  ;; pass in new state after running c1
           [razed (tag payload) (list (razed tag payload) state)])))]))

;; Compose many computations
(define-syntax let/eff*
  (syntax-rules ()
    [(_ () c) c]
    [(_ ([x c1] [x* c1*] ...) c2)
     (let/eff ([x c1])
              (let/eff* ([x* c1*] ...) c2))]))