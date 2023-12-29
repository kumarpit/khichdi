#lang plai

(require racket/stream)
(define (... . args) (cons '... args)) ;; enables us to use ... in templates

(print-only-errors #t)

;; Ralph
;; an language for specifying and operating on graphs

;; This file defines the interpreter for the Ralph language.

;; RID is Symbol
;; INVARIANT: a RID cannot be equal to any Ralph keyword
;; interp. a Ralph identifier
(define (rid? x)
  (local [(define RESERVED '(+ - with fun if0 pair left right mt array graph dict lookup))]
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
  [if0 (predicate Ralph?) (consequent Ralph?) (alternative Ralph?)]
  [pair (left Ralph?) (right Ralph?)]
  [left (e Ralph?)]
  [right (e Ralph?)]
  [array (e list?)]
  [aref (e Ralph?) (n number?)]
  [dict (e list?)]
  [lookup (e Ralph?) (n number?)]
  [entry (n number?) (e Ralph?)]
  [mt]
  [graph (nodeset Ralph?) (edgeset Ralph?)])

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
;; (PAIRS/LISTS/DICTIONARIES)
;;        | {pair <Ralph> <Ralph>}
;;        | {left <Ralph>}
;;        | {right <Ralph>}
;;        | {array <Ralph>*}
;;        | {aref <Ralph> <num>}
;;        | {dict {<num> <Ralph>}*}
;;        | {lookup <Ralph> <num>}
;;        | {mt}
;; (GRAPHS)
;;        | {graph <Ralph> <Ralph>}
;; where
;; {with {x named} body} ≡ { {fun {x} body} named}

;; Syntactic Sugar
(define (with x named body) (app (fun x body) named))


;; 1 --> 2 --> 3
;; adjacency list representation
`{with {nodes {array 1 2 3}} 
       {with {edges {dict {1 : {array 2}}
                          {2 : {array 3}}
                          {3 : {mt} }}
                    {graph nodes edges}}}}

;; 1 --> 2 --> 3
;; adjacency matrix representation
`{with {nodes {array 1 2 3}}
       {with {nodes->index {dict {1 : 0}
                                 {2 : 1}
                                 {3 : 2}}}
             {with {'mtx {array {array 0 1 0}
                                {array 0 0 1}
                                {array 0 0 0}}}
                   {graph nodes edges}}}}


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
              (fn-for-ralph a))]
    [pair (l r) (... (fn-for-ralph l)
                     (fn-for-ralph r))]   
    [left (e) (... (fn-for-ralph e))]
    [right (e) (... (fn-for-ralph e))]
    [array (e) (... (fn-for-loralph e))]
    [aref (e n) (... (fn-for-ralph e)
                     n)]
    [mt () (...)]
    [graph (l r) (... (fn-for-ralph l)
                      (fn-for-ralph r))]))

#;
(define (fn-for-loralph r)
  (cond [(empty? r) ...]
        [else (... (fn-for-ralph (first r))
                   (fn-for-loralph (rest r)))]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpretation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;