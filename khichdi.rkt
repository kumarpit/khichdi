#lang plai

(require "env.rkt")       ;; env implementation
(require "abstract.rkt")  ;; effect abstraction implementation
(define (... . args) (cons '... args)) ;; enables us to use ... in templates

(print-only-errors #t)

;; Khichdi
;; a toy programming language with
;; - Mutable variables (by-reference and by-value)
;; - Mutable boxes
;; - First-class (recursive) functions
;; - Arrays, dictionaries and pairs
;; - Backstops (more on this later)
;; - Generalized search
;; - Exceptions
;; - Continuations

;; This file defines the interpreter for the Khichdi language.

;; RID is Symbol
;; INVARIANT: a RID cannot be equal to any Khichdi keyword
;; interp. a Ralph identifier
(define (rid? x)
  (local [(define RESERVED '(+ - with fun if0 pair left right mt array dict lookup))]
    (and (symbol? x)
         (not (memq x RESERVED)))))

(define RID0 'a)
(define RID1 'b)

;; No template: atomic data


(define-type Khichdi
  [num (n number?)]
  [add (lhs Khichdi?) (rhs Khichdi?)]
  [sub (lhs Khichdi?) (rhs Khichdi?)]
  [id (name rid?)] 
  [fun (param rid?) (body Khichdi?)]
  [app (rator Khichdi?) (arg Khichdi?)]
  [if0 (predicate Khichdi?) (consequent Khichdi?) (alternative Khichdi?)])

;; interp. expressions in an eager language that supports
;; arithmetic, functions, conditionals, and exceptions.
;; Its syntax is defined by the following BNF:
;; <Khichdi> ::=
;; (ARITHMETIC)
;;          <num>
;;        | {+ <Khichdi> <Khichdi>}
;;        | {- <Khichdi> <Khichdi>}
;; (IDENTIFIERS)
;;        | {with {<id> <Khichdi>} <Khichdi>}
;;        | <id>
;; (CONDITIONALS)
;;        | {if0 <Khichdi> <Khichdi> <Khichdi>}
;; (FUNCTIONS)
;;        | {<Khichdi> <Khichdi>}
;;        | {fun {<id>} <Khichdi>}
;; where
;; {with {x named} body} ≡ { {fun {x} body} named}

;; Syntactic Sugar
(define (with x named body) (app (fun x body) named))


#;
(define (fn-for-khichdi f)
  (type-case Khichdi f
    [num (n) (... n)]
    [add (l r) (... (fn-for-khichdi l)
                    (fn-for-khichdi r))]
    [sub (l r) (... (fn-for-khichdi l)
                    (fn-for-khichdi r))]
    [id (x) (... x)]
    [fun (x body) (... x
                       (fn-for-khichdi body))]
    [app (rator rand) (... (fn-for-khichdi rator)
                           (fn-for-khichdi rand))]
    [if0 (p c a)
         (... (fn-for-khichdi p)
              (fn-for-khichdi c)
              (fn-for-khichdi a))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpretation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Effect Abstraction (i.e Monads)

;; (let-env/eff ([env]) e) - bind f to the current accumulator and evalute e
;; (with-env/eff env e)    - run e with the accumulator set to env for its dynamic
;;                           extent

;; The Generic Interface Components:
;; (return/eff e)               - return the value of e
;; (run/eff e env0)             - run computation e, starting with accumulator env0
;; (let/eff ([x e1] e2))        - bind x to the value of e1 in e2, threading factor
;; (let/eff* ([x* e1*] ...) e2) - sequentialize instances of let/eff


;; Computation is Env -> Value
;; interp.  a function that awaits a standard accumulator and produces a value

;; End of Effect Abstraction


;; Interpreter values

(define-type Value
  [numV (n number?)]
  [funV (param symbol?) (body Khichdi?) (env procedure?)])

;; interp. Khichdi runtime values

#;
(define (fn-for-value v)
  (type-case Value v
    [numV (n) (... n)]
    [funV (param body env) (... param
                                (fn-for-ralph body)
                                env)]))

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


;; Ralph -> Computation
;; consumes a Ralph and produces the correspoding Value
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
                       (return/eff av)))]))


;; Ralph -> Value
;; produce the result of interpreting the given Ralph expression
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
