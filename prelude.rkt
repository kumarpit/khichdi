#lang racket

(require plai/datatype)
(require rackunit)
(provide (all-defined-out)
         (all-from-out plai/datatype)
         (all-from-out rackunit))

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


;; Location is symbol
;; interp. an address in a store.
(define location? symbol?)

;; No template: atomic data

(define-type Khichdi
  [num (n number?)]
  [add (lhs Khichdi?) (rhs Khichdi?)]
  [sub (lhs Khichdi?) (rhs Khichdi?)]
  [id (name kid?)] 
  [fun (param kid?) (body Khichdi?)]
  [app (rator Khichdi?) (arg Khichdi?)]
  [if0 (predicate Khichdi?) (consequent Khichdi?) (alternative Khichdi?)]
  [fix (name kid?) (body Khichdi?)]
  [newbox (e Khichdi?)]
  [setbox (e1 Khichdi?) (e2 Khichdi?)]
  [openbox (e Khichdi?)]
  [setvar (id kid?) (e Khichdi?)]
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
;; (RECURSION)
;;        | {fix <id> <Khichdi>}
;;        | {fixFun <id> <id> <Khichdi>}
;;        | {rec {<id> <Khichdi>} <Khichdi>}
;; (BOXES)
;;        | {newbox <Khichdi>}
;;        | {setbox <Khichdi> <Khichdi>}
;;        | {openbox <Khichdi>}
;; (SEQUENCING)
;;        | {seqn <Khichdi> <Khichdi>}
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
(define (fixFun f x body) (fix f (fun x body)))
(define (rec name named-expr body)
  (with name (fix name named-expr) body))
(define (seqn e1 e2) (with (gensym) e1 e2))

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
    [fix (x body) (... x
                       (fn-for-khichdi body))]
    [newbox (e) (... (fn-for-khichdi e))]
    [setbox (e1 e2) (... (fn-for-khichdi e1)
                         (fn-for-khichdi e2))]
    [openbox (e) (... (fn-for-khichdi e))]
    [boxV (e) (fn-for-khichdi e)]
    [match/handle (expr vid vbody etag eid ebody)
                  (... (fn-for-khichdi expr)
                       vid
                       (fn-for-khichdi vbody)
                       etag
                       eid
                       (fn-for-khichdi ebody))]
    [raze (tag expr) (... tag
                          (fn-for-khichdi expr))]))

;; Khichdi KID Khichdi -> Khichdi
;; substitute khichdi0 for x0 in khichdi
(define (subst khichdi x0 khichdi0)
  (local [;; Khichdi KID -> Khichdi
          ;; substitute khichdi0 for x0 in e if binder x is not x0
          (define (maybe-subst-in e x)
            (if (symbol=? x x0)
                e
                (recur e)))

          ;; Khichdi -> Khichdi
          (define (recur expr)
            (type-case Khichdi expr
              [num (n) (num n)]
              [add (l r) (add (recur l)
                              (recur r))]
              [sub (l r) (sub (recur l)
                              (recur r))]
              [id (x) (if (symbol=? x0 x)
                          khichdi0
                          (id x))]
              [fun (x body) (fun x (maybe-subst-in body x))]
              [app (rator rand) (app (recur rator)
                                     (recur rand))]
              [if0 (p c a)
                   (if0 (recur p)
                        (recur c)
                        (recur a))]
              [fix (x body) (fix x (maybe-subst-in body x))]
              [newbox (e) (newbox e)]
              [setbox (e1 e2) (setbox e1 e2)]
              [openbox (e) (openbox e)]
              [setvar (x e) (setvar x (maybe-subst-in e x))]
              [match/handle (expr vid vbody etag eid ebody)
                            (match/handle (recur expr)
                                          vid
                                          (maybe-subst-in vbody vid)
                                          etag
                                          eid
                                          (maybe-subst-in ebody eid))]
              [raze (tag expr) (raze tag
                                     (recur expr))]))]
    (recur khichdi)))


;; Interpreter values
(define-type Value
  [numV (n number?)]
  [funV (param symbol?) (body Khichdi?) (env procedure?)]
  [boxV (l location?)])

;; Khichdi canonical values
(define-type Canonical
  [value (v Value?)]
  [razed (tag tag?) (payload Value?)])
