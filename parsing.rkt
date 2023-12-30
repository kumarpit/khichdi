#lang plai

(require "ralph.rkt")
(print-only-errors #t)

;; This file defines the parser for the Ralph language.

;; Ralph-focused-sexp (RFS) is one of:
;; - number
;; - `{+ ,RFS ,RFS}
;; - `{- ,RFS ,RFS}
;; - `{with {,identifier ,RFS} ,RFS}
;; - identifier
;; - `{if0 ,RFS ,RFS ,RFS}
;; - `{,RFS ,RFS}
;; - `{fun {,identifer} ,RFS}
;; - "any other s-expression"
;; where identifier is any symbol except +, -, with, if0, fun
;; interp. any s-expression but with a focus on those that represent Ralph expressions

;; LORFS is one of:
;; - `{}
;; - `{,RFS . ,LORFS}
;; - "any other s-expression"

;; TEMPLATES

#;
(define (fn-for-rfs sexp)
  (match sexp
    [`,n #:when (number? n) (... n)]
    [`{+ ,sexp1 ,sexp2}
     (... (fn-for-rfs sexp1)
          (fn-for-rfs sexp2))]
    [`{- ,sexp1 ,sexp2}
     (... (fn-for-rfs sexp1)
          (fn-for-rfs sexp2))]
    [`{with {,x ,sexp1} ,sexp2} #:when (rid? x)
                                (... x
                                     (fn-for-rfs sexp1)
                                     (fn-for-rfs sexp2))]
    [`,x #:when (rid? x) (... x)]
    [`{if0 ,sexp1 ,sexp2 ,sexp3}
     (... (fn-for-rfs sexp1)
          (fn-for-rfs sexp2)
          (fn-for-rfs sexp3))]
    [`{,sexp1 ,sexp2}
     (... (fn-for-rfs sexp1)
          (fn-for-rfs sexp2))]
    [`{fun {,x} ,sexp} #:when (rid? x)
                       (... (fn-for-rfs sexp))]
    [otherwise (...)]))

#;
(define (fn-for-lorfs sexp)
  (match sexp
    [`{} (...)]
    [`{,sexp1 . ,sexp2} (... (fn-for-rfs sexp1)
                             (fn-for-lorfs sexp2))]
    [otherwise (...)]))


;; RFS -> Ralph
;; produce Ralph corresponding the given RFS expression
;; EFFECT: signals an error if the given RFS does not represent a valid Ralph expression
(define (parse/ralph sexp)
  (local (;; LORFS -> (listof Ralph)
          ;; parse a list of RFS expressions to a list of Ralph expressions
          ;; EFFECT: signals an error if encounters an invalid RFS/LORFS expression
          (define (parse/lorfs sexp)
            (match sexp
              [`{} empty]
              [`{,sexp1 . ,sexp2} (cons (parse/ralph sexp1)
                                        (parse/lorfs sexp2))]
              [otherwise (error 'parse/ralph "bad ralph: ~a" sexp)])))    
    (match sexp
      [`,n #:when (number? n) (num n)]
      [`{+ ,sexp1 ,sexp2}
       (add (parse/ralph sexp1)
            (parse/ralph sexp2))]
      [`{- ,sexp1 ,sexp2}
       (sub (parse/ralph sexp1)
            (parse/ralph sexp2))]
      [`{with {,x ,sexp1} ,sexp2} #:when (rid? x)
                                  (with x
                                        (parse/ralph sexp1)
                                        (parse/ralph sexp2))]
      [`,x #:when (rid? x) (id x)]
      [`{if0 ,sexp1 ,sexp2 ,sexp3}
       (if0 (parse/ralph sexp1)
            (parse/ralph sexp2)
            (parse/ralph sexp3))]
      [`{fun {,x} ,sexp} #:when (rid? x)
                         (fun x (parse/ralph sexp))]
      ;; function application is the last case to match
      [`{,sexp1 ,sexp2}
       ((parse/ralph sexp1)
        (parse/ralph sexp2))]
      [otherwise (error 'parse/ralph "bad ralph: ~a" sexp)])))


;; Examples
(test (parse/ralph 2) (num 2))
(test (parse/ralph '{+ 1 2}) (add (num 1) (num 2)))
(test (parse/ralph '{with {x 2} {+ x 3}}) (app (fun 'x (add (id 'x) (num 3))) (num 2)))
(test (parse/ralph '{with {x 3} {with {y 4} {+ x y}}})
      (app (fun 'x (app (fun 'y (add (id 'x) (id 'y))) (num 4))) (num 3)))
(test (parse/ralph '{with {x 0} {if0 x 1 {+ 2 x}}})
      (app (fun 'x (if0 (id 'x) (num 1) (add (num 2) (id 'x)))) (num 0)))
