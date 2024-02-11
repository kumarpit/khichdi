#lang plai

(require "khichdi.rkt")
(print-only-errors #t)

;; This file defines the parser for the Khichdi language.

;; Khichdi-focused-sexp (RFS) is one of:
;; - number
;; - `{+ ,KFS ,KFS}
;; - `{- ,KFS ,KFS}
;; - `{with {,identifier ,KFS} ,KFS}
;; - identifier
;; - `{if0 ,KFS ,KFS ,KFS}
;; - `{,KFS ,KFS}
;; - `{fun {,identifer} ,KFS}
;; - "any other s-expression"
;; where identifier is any symbol except +, -, with, if0, fun
;; interp. any s-expression but with a focus on those that represent Khichdi expressions

;; LOKFS is one of:
;; - `{}
;; - `{,KFS . ,LOKFS}
;; - "any other s-expression"

;; TEMPLATES


(define (fn-for-kfs sexp)
  (match sexp
    [`,n #:when (number? n) (... n)]
    [`{+ ,sexp1 ,sexp2}
     (... (fn-for-kfs sexp1)
          (fn-for-kfs sexp2))]
    [`{- ,sexp1 ,sexp2}
     (... (fn-for-kfs sexp1)
          (fn-for-kfs sexp2))]
    [`{with {,x ,sexp1} ,sexp2} #:when (rid? x)
                                (... x
                                     (fn-for-kfs sexp1)
                                     (fn-for-kfs sexp2))]
    [`,x #:when (rid? x) (... x)]
    [`{if0 ,sexp1 ,sexp2 ,sexp3}
     (... (fn-for-kfs sexp1)
          (fn-for-kfs sexp2)
          (fn-for-kfs sexp3))]
    [`{,sexp1 ,sexp2}
     (... (fn-for-kfs sexp1)
          (fn-for-kfs sexp2))]
    [`{fun {,x} ,sexp} #:when (rid? x)
                       (... (fn-for-kfs sexp))]
    [otherwise (...)]))

#;
(define (fn-for-lokfs sexp)
  (match sexp
    [`{} (...)]
    [`{,sexp1 . ,sexp2} (... (fn-for-kfs sexp1)
                             (fn-for-lokfs sexp2))]
    [otherwise (...)]))


;; KFS -> Khichdi
;; produce Khichdi corresponding the given KFS expression
;; EFFECT: signals an error if the given KFS does not represent a valid Khichdi expression
(define (parse/khichdi sexp)
  (local (;; LOKFS -> (listof Khichdi)
          ;; parse a list of RFS expressions to a list of Khichdi expressions
          ;; EFFECT: signals an error if encounters an invalid KFS/LOKFS expression
          (define (parse/lokfs sexp)
            (match sexp
              [`{} empty]
              [`{,sexp1 . ,sexp2} (cons (parse/khichdi sexp1)
                                        (parse/lokfs sexp2))]
              [otherwise (error 'parse/khichdi "bad Khichdi: ~a" sexp)])))    
    (match sexp
      [`,n #:when (number? n) (num n)]
      [`{+ ,sexp1 ,sexp2}
       (add (parse/khichdi sexp1)
            (parse/khichdi sexp2))]
      [`{- ,sexp1 ,sexp2}
       (sub (parse/khichdi sexp1)
            (parse/khichdi sexp2))]
      [`{with {,x ,sexp1} ,sexp2} #:when (rid? x)
                                  (with x
                                        (parse/khichdi sexp1)
                                        (parse/khichdi sexp2))]
      [`,x #:when (rid? x) (id x)]
      [`{if0 ,sexp1 ,sexp2 ,sexp3}
       (if0 (parse/khichdi sexp1)
            (parse/khichdi sexp2)
            (parse/khichdi sexp3))]
      [`{fun {,x} ,sexp} #:when (rid? x)
                         (fun x (parse/khichdi sexp))]
      ;; function application is the last case to match
      [`{,sexp1 ,sexp2}
       ((parse/khichdi sexp1)
        (parse/khichdi sexp2))]
      [otherwise (error 'parse/ralph "bad ralph: ~a" sexp)])))


;; Examples
(test (parse/khichdi 2) (num 2))
(test (parse/khichdi '{+ 1 2}) (add (num 1) (num 2)))
(test (parse/khichdi '{with {x 2} {+ x 3}}) (app (fun 'x (add (id 'x) (num 3))) (num 2)))
(test (parse/khichdi '{with {x 3} {with {y 4} {+ x y}}})
      (app (fun 'x (app (fun 'y (add (id 'x) (id 'y))) (num 4))) (num 3)))
(test (parse/khichdi '{with {x 0} {if0 x 1 {+ 2 x}}})
      (app (fun 'x (if0 (id 'x) (num 1) (add (num 2) (id 'x)))) (num 0)))
