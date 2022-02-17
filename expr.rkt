#lang racket
(require racket/match)
(require syntax/location)
(require syntax/srcloc)
(require racket/syntax-srcloc)
(require racket/trace)

(define blah #'foobar)


;; AST Nodes
(struct SrcLoc (line-no col-no span) #:transparent)
(struct WithSrc (src-loc body) #:transparent)
(struct Lam (var body) #:transparent)
(struct App (rator rand) #:transparent)
(struct Ann (expr type) #:transparent)

(struct Arrow (domain range) #:transparent)

(define (symbols? ls)
  (cond
    [(not (list? ls)) #f]
    [else (andmap (lambda (s) (symbol? s)) ls)]))

(define (applyLams vars body)
  (foldr (lambda (v acc) (Lam v acc)) body vars))

(define (parse-expr s)
   (match s
     [(? syntax? syn) (WithSrc (SrcLoc (syntax-line syn) (syntax-column syn) (syntax-span syn)) (parse-expr (syntax->datum syn)))]
     [`(λ ,(? symbol? x) => ,e) (Lam x (parse-expr e))]
     [`(λ ,(? symbols? vars) => ,e) (applyLams vars (parse-expr e))]
     [`(λ ,e) (error
              'parse-error
              (string-join
               '("got ~a"
                "lambdas are of the form: '(λ (x y) => z)") "\n") s)]
     [`(,e1 ,e2) (App (parse-expr e1) (parse-expr e2))]
     [(? symbol? x) x]
     [`(,e : ,t) (Ann (parse-expr e) (parse-type t))]
     [`(,e1 ,e2 ,e3 ,r ...) (parse-expr `((,e1 ,e2) ,e3 ,@r))]))

;; (trace parse-expr)

(define (parse-type t)
  t)