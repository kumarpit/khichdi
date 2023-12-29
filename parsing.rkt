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
;; - `{pair ,RFS ,RFS}
;; - `{left ,RFS}
;; - `{right ,RFS}
;; - `{array ,@LORFS}
;; - `{aref ,RFS <num>}
;; - `{dict ,@LOEFS}
;; - `{lookup <Ralph> <num>}
;; - `{mt}
;; - `{graph ,RFS ,RFS}
;; - "any other s-expression"
;; where identifier is any symbol except +, -, with, if0, fun, pair, left, right, array, aref
;;                                       mt, graph, dict, lookup
;; interp. any s-expression but with a focus on those that represent Ralph expressions

;; EFS is one of:
;; - `{<num> ,RFS}
;; - "any other s-expression"
;; interp. any s-expression but with a focus on those that represent Ralph dict entries

;; LORFS is one of:
;; - `{}
;; - `{,RFS . ,LORFS}
;; - "any other s-expression"

;; LOEFS is one of:
;; - `{}
;; - `{,EFS . ,LOEFS}
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
    [`{pair ,sexp1, sexp2}
     (... (fn-for-rfs sexp1)
          (fn-for-rfs sexp2))]
    [`{left ,sexp}
     (... (fn-for-rfs sexp))]
    [`{right sexp}
     {... (fn-for-rfs sexp)}]
    [`{array ,@lorfs}
     (... (fn-for-lorfs lorfs))]
    [`{aref ,sexp ,n} #:when (number? n)
                      (... (fn-for-rfs sexp)
                           n)]
    [`{dict ,@loefs}
     (... (fn-for-loefs loefs))]
    [`{lookup ,sexp ,n} #:when (number? n)
                        (... (fn-for-rfs sexp)
                             n)]
    [`{mt} (... `mt)]
    [`{graph ,sexp1 ,sexp2}
     (... (fn-for-rfs sexp1)
          (fn-for-rfs sexp2))]
    [otherwise (...)]))

#;
(define (fn-for-lorfs sexp)
  (match sexp
    [`{} (...)]
    [`{,sexp1 . ,sexp2} (... (fn-for-rfs sexp1)
                             (fn-for-lorfs sexp2))]
    [otherwise (...)]))

#;
(define (fn-for-efs efs)
  (match efs
    [`{,n ,sexp} #:when (number? n)
                 (...n
                  (fn-for-rfs (sexp)))]
    [otherwise (...)]))


#;
(define (fn-for-loefs sexp)
  (match sexp
    [`{} (...)]
    [`{,sexp1 . ,sexp2} (... (fn-for-efs sexp1)
                             (fn-for-lefs sexp2))]
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
                [otherwise (error 'parse/ralph "bad ralph: ~a" sexp)]))

          ;; EFS -> Ralph
          ;; parses a Ralph dict entry
          ;; EFFECT: signals an error if malformed s-exp
          (define (parse/efs efs)
            (match efs
              [`{,n : ,sexp} #:when (number? n)
                             (entry n (parse/ralph sexp))]
              [otherwise (error 'parse/ralph "bad ralph ~a" efs)]))

          ;; LOEFS -> (listof Ralph)
          ;; parses list of Ralph dict entries
          ;; EFFECT: signals an error if invalid s-exp
          (define (parse/loefs sexp)
            (match sexp
              [`{} empty]
              [`{,sexp1 . ,sexp2} (cons (parse/efs sexp1)
                                        (parse/loefs sexp2))]
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
      [`{pair ,sexp1, sexp2}
       (pair (parse/ralph sexp1)
             (parse/ralph sexp2))]
      [`{left ,sexp}
       (left (parse/ralph sexp))]
      [`{right ,sexp}
       {right (parse/ralph sexp)}]
      [`{array ,@lorfs} (array (parse/lorfs lorfs))]
      [`{aref ,sexp ,n} #:when (number? n)
                        (aref (parse/ralph sexp) n)]
      [`{dict ,@loefs} (dict (parse/loefs loefs))]
      [`{lookup ,sexp ,n} #:when (number? n)
                          (lookup (parse/ralph sexp) n)]
      [`{mt} (mt)]
      [`{graph ,sexp1 ,sexp2}
       (graph (parse/ralph sexp1)
              (parse/ralph sexp2))]
      ;; function application is the last case to match
      [`{,sexp1 ,sexp2}
       ((parse/ralph sexp1)
        (parse/ralph sexp2))]
      [otherwise (error 'parse/ralph "bad ralph: ~a" sexp)])))


;; Examples
(test (parse/ralph 2) (num 2))
(test (parse/ralph '{+ 1 2}) (add (num 1) (num 2)))
(test (parse/ralph '{with {x 2} {+ x 3}}) (app (fun 'x (add (id 'x) (num 3))) (num 2)))
(test (parse/ralph '{array 1}) (array (list (num 1))))
(test (parse/ralph '{array 4 5 6}) (array (list (num 4) (num 5) (num 6))))
(test (parse/ralph '{dict {1 : 2} {2 : 1} {3 : {array 1 2 3}}})
        (dict (list (entry 1 (num 2))
                    (entry 2 (num 1))
                    (entry 3 (array (list (num 1) (num 2) (num 3)))))))
(test (parse/ralph '{with {arr {array 1 2 3}} {aref arr 0}})
        (app (fun 'arr (aref (id 'arr) 0)) (array (list (num 1) (num 2) (num 3)))))
(test (parse/ralph '{with {x 3} {with {y 4} {+ x y}}})
        (app (fun 'x (app (fun 'y (add (id 'x) (id 'y))) (num 4))) (num 3)))
(test (parse/ralph '{with {edges {dict {1 : {array 2}}
                                         {2 : {array 3}}
                                         {3 : {mt}}}}
                            {graph nodes edges}})
      (app (fun 'edges (graph (id 'nodes) (id 'edges)))
           (dict (list (entry 1 (array (list (num 2))))
                       (entry 2 (array (list (num 3))))
                       (entry 3 (mt))))))
(test (parse/ralph
         '{with {nodes {array 1 2 3}}
                {with {edges {dict {1 : {array 2}}
                                   {2 : {array 3}}
                                   {3 : {mt}}}}
                      {graph nodes edges}}})
        (app (fun 'nodes (app (fun 'edges (graph (id 'nodes) (id 'edges)))
                              (dict (list (entry 1 (array (list (num 2))))
                                          (entry 2 (array (list (num 3))))
                                          (entry 3 (mt))))))
             (array (list (num 1) (num 2) (num 3)))))