#lang plai

(define (... . args) (cons '... args)) ;; enables us to use ... in templates

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

;; KID is Symbol
;; INVARIANT: a KID cannot be equal to any Khichdi keyword
;; interp. a Khichdi identifier
(define (kid? x)
  (local [(define RESERVED '(+ - with fun if0 pair left right mt array dict lookup))]
    (and (symbol? x)
         (not (memq x RESERVED)))))

(define RID0 'a)
(define RID1 'b)

;; No template: atomic data


;; Tag is Symbol
;; interp. an exception tag
(define (tag? x)
  (symbol? x))

(define tag0 'a)
(define tag1 'out-of-bounds-exception)

;; No template: atomic data

(define-type Khichdi
  [num (n number?)]
  [add (lhs Khichdi?) (rhs Khichdi?)]
  [sub (lhs Khichdi?) (rhs Khichdi?)]
  [id (name kid?)] 
  [fun (param kid?) (body Khichdi?)]
  [app (rator Khichdi?) (arg Khichdi?)]
  [if0 (predicate Khichdi?) (consequent Khichdi?) (alternative Khichdi?)]
  [raze (tag tag?) (expr Khichdi?)]
  [match/handle (expr Khichdi?)
                (value-id kid?) (value-body Khichdi?)
                (exn-tag tag?) (exn-id kid?) (exn-body Khichdi?)])

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
;; (EXCEPTIONS)
;;        | {match/handle <Khichdi>
;;           [<id> <Khichdi>]
;;           [{raze <tag> <id>} <Khichdi>]}
;;        | {raze <tag> <Khichdi>}
;; where
;; {with {x named} body} ≡ { {fun {x} body} named}
;; <tag> is any symbol

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
              (fn-for-khichdi a))]
    [match/handle (expr vid vbody etag eid ebody)
                  (... (fn-for-khichdi expr)
                       vid
                       (fn-for-khichdi vbody)
                       etag
                       eid
                       (fn-for-khichdi ebody))]
    [raze (tag expr) (... tag
                          (fn-for-khichdi expr))]))

;; Interpreter values
(define-type Value
  [numV (n number?)]
  [funV (param symbol?) (body Khichdi?) (env procedure?)])

;; Khichdi canonical values
(define-type Canonical
  [value (v Value?)]
  [razed (tag tag?) (payload Value?)])
