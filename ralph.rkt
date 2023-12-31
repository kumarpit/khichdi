#lang plai

(require "env.rkt")
(define (... . args) (cons '... args)) ;; enables us to use ... in templates

(print-only-errors #t)

;; Ralph
;; a toy programming language with
;; - Mutable variables (by-reference and by-value)
;; - Mutable boxes
;; - First-class (recursive) functions
;; - Arrays, dictionaries and pairs
;; - Backstops (more on this later)
;; - Generalized search
;; - Exceptions
;; - Continuations

;; This file defines the interpreter for the Ralph language.

;; RID is Symbol
;; INVARIANT: a RID cannot be equal to any Ralph keyword
;; interp. a Ralph identifier
(define (rid? x)
  (local [(define RESERVED '(+ - with fun if0 pair left right mt array dict lookup))]
    (and (symbol? x)
         (not (memq x RESERVED)))))

(define RID0 'a)
(define RID1 'b)

;; No template: atomic data


(define-type Ralph
  [num (n number?)]
  [add (lhs Ralph?) (rhs Ralph?)]
  [sub (lhs Ralph?) (rhs Ralph?)]
  [id (name rid?)] 
  [fun (param rid?) (body Ralph?)]
  [app (rator Ralph?) (arg Ralph?)]
  [if0 (predicate Ralph?) (consequent Ralph?) (alternative Ralph?)])

;; interp. expressions in an eager language that supports
;; arithmetic, functions, conditionals, and exceptions.
;; Its syntax is defined by the following BNF:
;; <Ralph> ::=
;; (ARITHMETIC)
;;          <num>
;;        | {+ <Ralph> <Ralph>}
;;        | {- <Ralph> <Ralph>}
;; (IDENTIFIERS)
;;        | {with {<id> <Ralph>} <Ralph>}
;;        | <id>
;; (CONDITIONALS)
;;        | {if0 <Ralph> <Ralph> <Ralph>}
;; (FUNCTIONS)
;;        | {<Ralph> <Ralph>}
;;        | {fun {<id>} <Ralph>}
;; where
;; {with {x named} body} ≡ { {fun {x} body} named}

;; Syntactic Sugar
(define (with x named body) (app (fun x body) named))


#;
(define (fn-for-ralph f)
  (type-case Ralph f
    [num (n) (... n)]
    [add (l r) (... (fn-for-ralph l)
                    (fn-for-ralph r))]
    [sub (l r) (... (fn-for-ralph l)
                    (fn-for-ralph r))]
    [id (x) (... x)]
    [fun (x body) (... x
                       (fn-for-ralph body))]
    [app (rator rand) (... (fn-for-ralph rator)
                           (fn-for-ralph rand))]
    [if0 (p c a)
         (... (fn-for-ralph p)
              (fn-for-ralph c)
              (fn-for-ralph a))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpretation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Interpreter values

(define-type Value
  [numV (n number?)]
  [funV (param symbol?) (body Ralph?) (env procedure?)])

;; interp. Ralph runtime values

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
    [funV (param body env) (error 'interp/ralph "bad number: ~a" v)]))


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


;; Value Value -> Value
;; produces the sum of the two numbers represented by the given values
;; EFFECT: signals an error if values cannot be converted to numbers
(define (add-value v1 v2)
  (num-binop-value + v1 v2))


;; Value Value -> Value
;; produces the difference of the two numbers represented by the given values
;; EFFECT: signals an error if values cannot be converted to numbers
(define (sub-value v1 v2)
  (num-binop-value - v1 v2))


;; Value Value -> Value
;; produces the result of applying v1 to v2
;; EFFECT: signals an error if v1 is not a function value
(define (apply-value v1 v2)
  (type-case Value v1
    [numV (n) (error 'interp/ralph "bad function: ~a" v1)]
    [funV (x body env) (interp/ralph-env body
                                             (extend-env env x v2))]))


;; Ralph Env -> Value
;; environment passing interpreter for Ralph
;; EFFECT: signals an error in case of a runtime error
(define (interp/ralph-env r env)
  (type-case Ralph r
    [num (n) (numV n)]
    [add (l r) (add-value (interp/ralph-env l env)
                          (interp/ralph-env r env))]
    [sub (l r) (sub-value (interp/ralph-env l env)
                          (interp/ralph-env r env))]
    [id (x) (lookup-env env x)]
    [fun (x body) (funV x body env)]
    [app (rator rand) (apply-value (interp/ralph-env rator env)
                                   (interp/ralph-env rand env))]
    [if0 (p c a)
         (if  (zero-value? (interp/ralph-env p env))
              (interp/ralph-env c env)
              (interp/ralph-env a env))]))


;; Ralph -> Value
;; produce the result of interpreting the given Ralph expression
;; EFFECT: signals an error in case of a runtime error
(define (interp/ralph r)
  (interp/ralph-env r empty-env))

(test (interp/ralph (num 2)) (numV 2))
(test (interp/ralph (app (fun 'x (add (id 'x) (num 3))) (num 2)))
      (numV 5))
(test (interp/ralph (app (fun 'x (app (fun 'y (add (id 'x) (id 'y))) (num 4))) (num 3)))
      (numV 7))
(test (interp/ralph (app (fun 'x (if0 (id 'x) (num 1) (add (num 2) (id 'x)))) (num 0)))
      (numV 1))
