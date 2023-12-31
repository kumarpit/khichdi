#lang plai

;; (let-env/eff ([env]) e) - bind f to the current accumulator and evalute e
;; (with-env/eff env e)    - run e with the accumulator set to env for its dynamic
;;                           extent

;; The Generic Interface Components:
;; (return/eff e)               - return the value of e
;; (run/eff e env0)             - run computation e, starting with accumulator env0
;; (let/eff ([x e1] e2))        - bind x to the value of e1 in e2, threading factor
;; (let/eff* ([x* e1*] ...) e2) - sequentialize instances of let/eff

;;
;; Effect Implementation
;;

;; Computation is Env -> Value
;; interp.  a function that awaits a standard accumulator and produces a value

(define-syntax let-env/eff
  (syntax-rules ()
    [(_ ([env]) c)
     (λ (env) (c env))]))


;; Env Computation -> Computation
;; run c in a context where the environment is set to env
(define (with-env/eff env c)
  (λ (env0) (c env)))

 
;; Value -> Computation
;; create a computation that yields n
(define (return/eff v)
  (λ (env) v))

;; Computation -> Natural
;; run the given computation and produce its result
(define (run/eff c env0)
  (c env0))

;; Compose two computations
(define-syntax let/eff
  (syntax-rules ()
    [(_ ([x c1]) c2)
     (λ (env)
       (let ([x (c1 env)])
         (c2 env)))]))

;; Compose many computations
(define-syntax let/eff*
  (syntax-rules ()
    [(_ () c) c]
    [(_ ([x c1] [x* c1*] ...) c2)
     (let/eff ([x c1])
              (let/eff* ([x* c1*] ...) c2))]))