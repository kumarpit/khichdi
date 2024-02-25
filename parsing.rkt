#lang plai

(require "prelude.rkt")
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
;; - `{fix ,identifier ,KFS}
;; - `{fixFun ,identifier ,identifier ,KFS}
;; - `{rec {,identifier, ,KFS} ,KFS}
;; - `{match/handle ,KFS
;;      [,identifier ,KFS]
;;      [{raze ,tag ,identifier} ,KFS]}
;; - `{raze ,tag ,KFS}
;; - "any other s-expression"
;; where identifier is any symbol except +, -, with, if0, fun
;; where tag is any symbol
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
    [`{with {,x ,sexp1} ,sexp2} #:when (kid? x)
                                (... x
                                     (fn-for-kfs sexp1)
                                     (fn-for-kfs sexp2))]
    [`,x #:when (kid? x) (... x)]
    [`{if0 ,sexp1 ,sexp2 ,sexp3}
     (... (fn-for-kfs sexp1)
          (fn-for-kfs sexp2)
          (fn-for-kfs sexp3))]
    [`{,sexp1 ,sexp2}
     (... (fn-for-kfs sexp1)
          (fn-for-kfs sexp2))]
    [`{fun {,x} ,sexp} #:when (kid? x)
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
      [`{with {,x ,sexp1} ,sexp2} #:when (kid? x)
                                  (with x
                                        (parse/khichdi sexp1)
                                        (parse/khichdi sexp2))]
      [`,x #:when (kid? x) (id x)]
      [`{if0 ,sexp1 ,sexp2 ,sexp3}
       (if0 (parse/khichdi sexp1)
            (parse/khichdi sexp2)
            (parse/khichdi sexp3))]
      [`{fun {,x} ,sexp} #:when (kid? x)
                         (fun x (parse/khichdi sexp))]
      [`{fix ,x ,sexp} #:when (kid? x) (fix x (parse/khichdi sexp))]
      [`{fixFun ,f {,x} ,sexp} #:when (and (kid? f)
                                           (kid? x))
                               (fixFun f x (parse/khichdi sexp))]
      [`{rec {,x ,sexp1} ,sexp2} #:when (kid? x) (rec x
                                                   (parse/khichdi sexp1)
                                                   (parse/khichdi sexp2))]                                                   
      [`{match/handle ,sexp1 [,x1 ,sexp2][{raze ,x2 ,x3} ,sexp3]}
       #:when (and (kid? x1)
                   (tag? x2)
                   (kid? x3))
       (match/handle (parse/khichdi sexp1)
                     x1
                     (parse/khichdi sexp2)
                     x2
                     x3
                     (parse/khichdi sexp3))]
      [`{raze ,x ,sexp} #:when (tag? x)
                        (raze x (parse/khichdi sexp))]
      [`{openbox ,sexp} (openbox (parse/khichdi sexp))]
      [`{newbox ,sexp} (newbox (parse/khichdi sexp))]
      [`{setbox ,sexp1 ,sexp2} (setbox (parse/khichdi sexp1)
                                       (parse/khichdi sexp2))]
      [`{seqn ,sexp1 ,sexp2} (seqn (parse/khichdi sexp1)
                                   (parse/khichdi sexp2))]
      [`{setvar ,x ,e} (setvar x (parse/khichdi e))]
      ;; function application is the last case to match
      [`{,sexp1 ,sexp2}
       (app (parse/khichdi sexp1)
            (parse/khichdi sexp2))]
      [otherwise (error 'parse/ralph "bad khichdi: ~a" sexp)])))


;; Examples
(test (parse/khichdi 2) (num 2))
(test (parse/khichdi `{f 2}) (app (id 'f) (num 2)))
(test (parse/khichdi '{+ 1 2}) (add (num 1) (num 2)))
(test (parse/khichdi '{with {x 2} {+ x 3}}) (app (fun 'x (add (id 'x) (num 3))) (num 2)))
(test (parse/khichdi '{with {x 3} {with {y 4} {+ x y}}})
      (app (fun 'x (app (fun 'y (add (id 'x) (id 'y))) (num 4))) (num 3)))
(test (parse/khichdi '{with {x 0} {if0 x 1 {+ 2 x}}})
      (app (fun 'x (if0 (id 'x) (num 1) (add (num 2) (id 'x)))) (num 0)))

(test (parse/khichdi '{fix f f}) (fix 'f (id 'f)))
(test (parse/khichdi '{fixFun f {x} {f 1}}) (fix 'f (fun 'x (app (id 'f) (num 1)))))
(test (parse/khichdi '{with {down {fix f {fun {x} {if0 x 9 {f {- x 1}}}}}} {down 1}})
      (app (fun 'down (app (id 'down) (num 1)))
           (fix 'f
                (fun 'x
                     (if0 (id 'x)
                          (num 9)
                          (app (id 'f) (sub (id 'x) (num 1))))))))
(test (parse/khichdi '{rec {x {x 2}} {x 1}}) (app
                                              (fun 'x (app (id 'x) (num 1)))
                                              (fix 'x (app (id 'x) (num 2)))))

(test (parse/khichdi '{raze some-tag 2}) (raze 'some-tag (num 2)))
(test (parse/khichdi '{match/handle {raze some-tag 2}
                                    [x {+ x 3}]
                                    [{raze some-tag x} {+ x 4}]})
      (match/handle (raze 'some-tag (num 2))
                    'x
                    (add (id 'x) (num 3))
                    'some-tag
                    'x
                    (add (id 'x) (num 4))))

(test (parse/khichdi '{newbox 2}) (newbox (num 2)))
#; 
(test (parse/khichdi '{with {x {newbox 2}}
                            {seqn {setbox x 3}
                                  {+ {openbox x} 1}}})
      (app (fun 'x (app (fun 'g1805382 (add (openbox (id 'x)) (num 1)))
                        (setbox (id 'x) (num 3))))
           (newbox (num 2))))
#;
(test (parse/khichdi '{with {x 2} {seqn {setvar x 3}
                                        {+ x 1}}})
      (app (fun 'x (app (fun 'g2627105 (add (id 'x) (num 1))) (setvar 'x (num 3)))) (num 2)))
