#lang pl

;;        2

#| The grammar: 
 <FLANG> ::= <num>                   ;; Rule 1 
 | { + <FLANG> <FLANG> }             ;; Rule 2 
 | { - <FLANG> <FLANG> }             ;; Rule 3 
 | { * <FLANG> <FLANG> }             ;; Rule 4 
 | { / <FLANG> <FLANG> }             ;; Rule 5 
 | { with { <id> <FLANG> } <FLANG> } ;; Rule 6 
 | <id>                              ;; Rule 7 
 | { fun { <id> } <FLANG> }          ;; Rule 8 
 | { call <FLANG> <FLANG> }          ;; Rule 9 
 | { True }     ;; add rule for True ;; Rule 10 
 | { False }                         ;; Rule 11 
 | { < <FLANG> <FLANG> } ;; add rule for = ;; Rule 12 
 | { > <FLANG> <FLANG> }                   ;; Rule 13 
 | { not <FLANG> }                         ;; Rule 14 
 | { = <FLANG> <FLANG> }                   ;; Rule 15 
 | "{if <FLANG> {then-do <FLANG>} {else-do <FLANG>}}") ;; add rule 16 for (the above) if 
expressions 
|#


;;               3

;==== AST definition ====
(define-type FLANG
            [Num Number]
            [Add FLANG FLANG]
            [Sub FLANG FLANG]
            [Mul FLANG FLANG]
            [Div FLANG FLANG]
            [With Symbol FLANG FLANG];name, named-expr, body
            [Id Symbol]
            [Fun Symbol FLANG]; parameter-name, body
            [Call FLANG FLANG]
            [Bool Boolean] 
            [Bigger FLANG FLANG] 
            [Smaller FLANG FLANG] 
            [Equal FLANG FLANG] 
            [Not FLANG] 
            [If FLANG FLANG FLANG]
)


#|
we take a sexpr and convert it to our language - to Flang expression we can "understand" in our rules
|#
(: parse-sexpr : Sexpr -> FLANG) 
 (define (parse-sexpr sexpr) 
 (match sexpr 
 [(number: n) (Num n)] 
 ['True (Bool true)] 
 ['False (Bool false)]
 [(symbol: name) (Id name)]
 [(list '+ l r) (Add (parse-sexpr l) (parse-sexpr r))]
 [(list '- l r) (Sub (parse-sexpr l) (parse-sexpr r))]
 [(list '* l r) (Mul (parse-sexpr l) (parse-sexpr r))]
 [(list '/ l r) (Div (parse-sexpr l) (parse-sexpr r))]
 [(cons 'with more)
          ( match sexpr
          [(list 'with (list (symbol: name) named-expr) body)
                                 (With name (parse-sexpr named-expr)
                                             (parse-sexpr body))]
             [else (error 'parse-sexpr "bad with syntax!!")])]
[(cons 'fun more)
    ( match sexpr
    [(list 'fun (list (symbol: name)) body)
                                 (Fun name (parse-sexpr body))]
             [else (error 'parse-sexpr "bad fun syntax")])]
    [(list 'call fun-expr arg-expr)
                                  (Call (parse-sexpr fun-expr)
                                        (parse-sexpr arg-expr))]
 [(list '= lhs rhs) (Equal (parse-sexpr lhs) (parse-sexpr rhs))]
 [(list '> lhs rhs) (Bigger (parse-sexpr lhs) (parse-sexpr rhs))]
 [(list '< lhs rhs) (Smaller (parse-sexpr lhs) (parse-sexpr rhs))]
 [(list 'not exp) (Not (parse-sexpr exp))]
 [(cons 'if body)(match sexpr[(list 'if condition  (list 'then-do then) (list 'else-do else))
                              (If (parse-sexpr condition) (parse-sexpr then) (parse-sexpr else))]
                   [else (error 'parse-sexpr "bad `if' syntax in ~s" sexpr)])]
 [else (error 'parse-sexpr "bad syntax in ~s" sexpr)])) 

(: parse : String -> FLANG)
(define (parse code)
       (parse-sexpr (string->sexpr code)))


;;      tests
(test (parse "4") => (Num 4))
(test (parse "{+ 3 5}") => (Add (Num 3) (Num 5)))
(test (parse "{+ 3 {- 8 {+ 2 1}}}") => (Add (Num 3) (Sub (Num 8) (Add (Num 2) (Num 1)))))
(test (parse "{+ 1 2 3}") =error> "bad syntax")
(test (parse "{with {x {+ 4 2}} {* x x}}") => (With 'x (Add (Num 4) (Num 2))
                                              (Mul (Id 'x) (Id 'x))))
(test (parse "{fun {x} x}") => (Fun 'x (Id 'x)))
(test (parse "{fun {x} {/ x 5}}") => (Fun 'x (Div (Id 'x) (Num 5))))
(test (parse "{call {fun {x} {/ x 5}} 8}") => (Call {Fun 'x (Div (Id 'x) (Num 5))} (Num 8)))
(test (parse "{with {sqr {fun {x} {* x x}}}
                                    {+ {call sqr 5}
                                        {call sqr 6}}}") =>
                 (With 'sqr (Fun 'x (Mul (Id 'x) (Id 'x)))
                       (Add (Call (Id 'sqr) (Num 5))
                            (Call (Id 'sqr) (Num 6)))))
(test (parse "{fun x {* x x}}")=error> "bad fun syntax")
(test (parse "{call {fun {x} {* x x}} 5}") => (Call (Fun 'x (Mul (Id 'x) (Id 'x))) (Num 5)))
(test (parse "{with {fun {x} {* x x}} {+ {call sqr 5} {call sqr 5}}}")
=error> "parse-sexpr: bad with syntax!!")
(test (parse "{call {fun {x} {+ x 0}} 0}") =>
(Call (Fun 'x (Add (Id 'x) (Num 0))) (Num 0)))


;;              4

#|
Formal Substitution rules: 
 
subst: 
 N[v/x] = N 
 {+ E1 E2}[v/x] = {+ E1[v/x] E2[v/x]} 
 {- E1 E2}[v/x] = {- E1[v/x] E2[v/x]} 
 {* E1 E2}[v/x] = {* E1[v/x] E2[v/x]} 
{/ E1 E2}[v/x] = {/ E1[v/x] E2[v/x]} 
y[v/x] = y 
x[v/x] = v 
 {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]} ; if y =/= x 
 {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2} 
 {call E1 E2}[v/x] = {call E1[v/x] E2[v/x]} 
 {fun {y} E}[v/x] = {fun {y} E[v/x]} ; if y =/= x 
 {fun {x} E}[v/x] = {fun {x} E} 
 B[v/x] = B ;; B is Boolean 
 {= E1 E2}[v/x] = {= E1[v/x] E2[v/x]} 
 {> E1 E2}[v/x] = {> E1[v/x] E2[v/x]} 
 {< E1 E2}[v/x] = {< E1[v/x] E2[v/x]} 
 { not E}[v/x] = {not E[v/x]} 
 {if Econd {then-do Edo} {else-do Eelse}}[v/x] 
 = {if Econd[v/x] {then-do Edo[v/x]} {else-do 
Eelse[v/x]}} 
|#


;; converting to flang with the corect symbols after rename them correctuly
(: subst : FLANG Symbol FLANG -> FLANG)
(define (subst expr from to)
        (cases expr
            [(Num n) expr]
            [(Add l r) (Add (subst l from to) (subst r from to))]
            [(Sub l r) (Sub (subst l from to) (subst r from to))]
            [(Mul l r) (Mul (subst l from to) (subst r from to))]
            [(Div l r) (Div (subst l from to) (subst r from to))]
            [(With name named body)
                      (With name (subst named from to)
                            (if (eq? from name)
                                body
                                (subst body from to)))]
            [(Fun name body)
                          (Fun name (if (eq? name from)
                              body
                              (subst  body from to)))]
            [(Call fun-expr arg-expr)  (Call (subst fun-expr from to) (subst arg-expr from to))]
            [(Id name) (if (eq? name from)
                           to
                           expr)]
            [(Bool b) expr]
            [(Not b) (Not (subst b from to))]
            [(Equal l r) (Equal (subst l from to) (subst r from to))]
            [(Bigger l r)(Bigger (subst l from to) (subst r from to))]
            [(Smaller l r)(Smaller (subst l from to) (subst r from to))]
            [(If condition than else)(If (subst condition from to) (subst than from to) (subst else from to))]
         ))


(test (subst (Mul (Id 'x) (Id 'x))
       'x
       (Num 6)
       ) => (Mul (Num 6) (Num 6)))
(test (subst (Id 'x)
             'x
             (Num 8)) => (Num 8))
(test (subst (Id 'y)
             'x
             (Num 8)) => (Id 'y))
(test (subst (With 'x (Num 3)
                   (Id 'x))
             'x
             (Num 5)) => (With 'x (Num 3)
                   (Id 'x)))
(test (subst (With 'y
                   (Add (Id 'x) (Num 3))
                   (Sub (Id 'x) (Num 5)))
             'x
             (Num 4)) => (With 'y
                               (Add (Num 4) (Num 3))
                               (Sub (Num 4) (Num 5))))
(test (subst (Fun 'x (Add (Id 'x) (Id 'y)))
             'x
             (Num 4)) => (Fun 'x (Add (Id 'x) (Id 'y))))
(test (subst (Fun 'x (Add (Id 'x) (Id 'y)))
             'y
             (Num 4)) => (Fun 'x (Add (Id 'x) (Num 4))))
(test (subst (Call (Fun 'x (Div (Id 'x) (Id 'y)))
                   (Add (Id 'x) (Id 'y)))
                   'x
                   (Num 3)) => (Call (Fun 'x (Div (Id 'x) (Id 'y)))
                   (Add (Num 3) (Id 'y))))
(test (subst (Call (Fun 'x (Div (Id 'x) (Id 'y)))
                   (Add (Id 'x) (Id 'y)))
                   'y
                   (Num 3)) => (Call (Fun 'x (Div (Id 'x) (Num 3)))
                   (Add (Id 'x) (Num 3))))
(test (subst (Bool #t) 'x (Num 8)) => (Bool #t))
(test (subst (Not (Bool #f)) 'x (Num 8)) => (Not (Bool #f)))
(test (subst (Equal (Bool #t) (Bool #f)) 'x (Num 8)) => (Equal (Bool #t) (Bool #f)))
(test (subst (Bigger (Bool #t) (Bool #f)) 'x (Num 8)) => (Bigger (Bool #t) (Bool #f)))
(test (subst (Smaller (Bool #t) (Bool #f)) 'x (Num 8)) => (Smaller (Bool #t) (Bool #f)))



;; take the arithmethic operation we want to do and return the representing flag after the op
(: arith-op : (Number Number -> Number) FLANG FLANG -> FLANG)
(define (arith-op op arg1 arg2)
        (: Num->Number : FLANG -> Number)
        (define (Num->Number arg)  
        (cases arg
          [(Num n) n]
          [else (error 'Num->Number "expected number, got: ~s" arg)]
         ))
     (Num (op (Num->Number arg1) (Num->Number arg2))))

;; take the logic operation we want to do and return the representing flag after the op
(: logic-op : (Number Number -> Boolean) FLANG FLANG -> FLANG)
(define (logic-op op expr1 expr2)
  (: Num->number : FLANG -> Number)
(define (Num->number e)
  (cases e
    [(Num n) n]
    [else (error 'Num->number "expected a number, got: ~s" e)]))
  (Bool (op (Num->number expr1) (Num->number expr2))))
 (: flang->bool : FLANG -> Boolean) 
 (define (flang->bool e) 
 (cases e
    [(Bool b) b]
    [else true]))


;;   evaluation of the flang 
(: eval : FLANG -> FLANG)
(define (eval expr)
        (cases expr
            [(Num n) expr]
            [(Add l r)  (arith-op + (eval l) (eval r))]
            [(Sub l r) (arith-op - (eval l) (eval r))]
            [(Mul l r) (arith-op * (eval l) (eval r))]
            [(Div l r) (arith-op / (eval l) (eval r))]
            [(With name named body) (eval (subst body name (eval named)))]
            [(Id name) (error 'eval "free identifier: ~s" name)]
            [(Fun name body) expr]
            [(Call fun-expr arg-expr) (let ([fval (eval fun-expr)])
                                        (cases fval
                                          [(Fun name body) (eval (subst body
                                                                        name
                                                                        (eval arg-expr)))]
                                          [else (error 'eval "expected a function, got: ~s" fval)]))]
            [(Bool b) expr]
            [(Not expr) (Bool (not (flang->bool (eval expr))))]
            [(Bigger first second) (logic-op > (eval first) (eval second))]
            [(Smaller  first second) (logic-op < (eval first) (eval second))]
            [(Equal first second) (logic-op = (eval first) (eval second))]
             [(If l m r)
             (cond
             [(flang->bool (eval l)) (eval m)]
             [else (eval r)])]
         )
  )

(test (eval (Call (Fun 'x (Mul (Id 'x) (Num 4)))
            (Num 3))) => (Num 12))
(test (eval (Call (With 'foo
                  (Fun 'x (Mul (Id 'x) (Num 4)))
                  (Id 'foo))
            (Num 3))) => (Num 12))
(test (eval (Call (Num 5) (Num 5))) =error> "eval: expected a function, got: #(struct:Num 5)")


(: run : String -> (U Number Boolean FLANG)) 
 (define (run str) 
 (let ([result (eval (parse str))]) 
(cases result 
 [(Num n) n] 
 [(Bool b) b]
 [else result]
  )))


        
         

(test (run "5") => 5)
(test (run "{+ 4 6}") => 10)
(test (run "{+ 4 6}") => 10)
(test (run "{call {fun {x} {+ x 7}} {with {x 8} {+ x 7}}}") => 22)
(test (run "{with {identity {fun {x} x}}
                      {with {foo {fun {x} {+ x 1}}}
                                    {call {call identity foo} 123}}}") => 124)

;; tests

(test (run "True") => true) 
(test (run "{not True}") => false) 
(test (run "{> 3 44}") => false)
(test (run "{= 3 3}") => true) 
(test (run "{if {- 3 3} {then-do 4} {else-do 5}}") => 4) 
(test (run "{with {x 8} 
 {if {> x 0} {then-do {/ 2 x}} {else-do x}}}") => 1/4) 
(test (run "{with {x 0} 
 {if {> x 0} {then-do {/ 2 x}} {else-do x}}}") => 0) 
(test (run "{if {> 2 1} {then-do True} {else-do {+ 2 2}}}") => true) 
(test (run "{with {c True} 
 {if c {then-do {> 2 1}} {else-do 2}}}") 
 => true)
(test (run "{with {foo {fun {x} 
 {if {< x 2} {then-do x} {else-do {/ x 2}}}}} foo}") 
 => (Fun 'x (If (Smaller (Id 'x) (Num 2)) (Id 'x) (Div (Id 'x) (Num 2))))) 
(test (run "{with {x 0} 
 {if {> x 0} {/ 2 x} x}}") 
 =error> "parse-sexpr: bad `if' syntax in (if (> x 0) (/ 2 x) x)") 
 (test (run "true") =error> "eval: free identifier: true") 
(test (run "{< false 5}") =error> "eval: free identifier: false") 
(test (run "{< False 5}") 
 =error> "Num->number: expected a number, got: #(struct:Bool #f)")
(test (run "{+ False 5}")
=error> "Num->Number: expected number, got: #(struct:Bool #f)")


