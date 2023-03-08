#lang pl
;;        1.a

#|
<SE> ::= <string> 1
       | {string <D> <string>} 2
       | {string-append <SE> <string>} 3
       | {string-length <string>} 4
       | {string-insert <string>} 5
       | {number->string <Number>} 6

(0)

<D> ::=  <#/0> 1
       | <#/1> 2
       | <#/2> 3
       | <#/3> 4
       | <#/4> 5
       | <#/5> 6
       | <#/6> 7
       | <#/7> 8
       | <#/8> 9
       | <#/9> 10
       | <D> <D> 11


(1)

<string> ::= <D>
            |{string <D>}

(3)

<string-append> ::= <string>
                   |{string-append <string> <string>}

(4)

<string-length> ::= <Number>
                   |{+ Number <SE> }
(5)

<string-insert> ::= <string> <D> <Number>

(6)

<number->string> ::= <D>
                    |<string>

|#

;;                1.b
#|

ex 1)

( string-append ( string-length "44" ) "12" ) 
( string-append ((4) 1 string-length "4" ) "12" )
( string-append ((4) 2 ) "12" )
( (3) 2 "12" )
( (3) (1) "12" )
( (1) "212" )

ex 2)

( string-insert "1357" 4 66 )

( string-insert (1) (0) 66 )

( (5) (1) (0) 66 )

( "1357" 4 66 )


ex 3)

( string #\1 #\2 #\4 )

( string (1) #\2 #\4 )

( string (1) #\4 )

( string (1))

( "124" )

|#

;;        2

(: square : Number -> Number)
(define (square num)
  (* num num)
 )

(: sum-of-squares : (Listof Number) -> Number)
(define (sum-of-squares lst)
  (foldl + 0 (map square lst)))

(test (sum-of-squares '(1 2 3)) => 14)
(test (sum-of-squares '(-1 -2 -3)) => 14)
(test (sum-of-squares '()) => 0)
(test (sum-of-squares '(-5)) => 25)
(test (sum-of-squares '(-5)) => 25)
(test (sum-of-squares '(0.2)) => 0.04000000000000001)
(test (sum-of-squares '(0 0 0)) => 0)
(test (sum-of-squares '(-4 2 0)) => 20)
(test (sum-of-squares '(10 0.1 0 -10)) => 200.01)


;;         3.a
;; create a func with the coeffs
(: createPolynomial : (Listof Number) -> (Number -> Number))
(define (createPolynomial coeffs)
  
;; adding x and pow and calculate in tail recursive way the function with the coeffs
(: poly : (Listof Number) Number Integer Number -> Number)
  (define (poly argsL x power accum)
     (if (null? argsL)
        accum
        (poly (rest argsL) x (+ power 1) (+ (* (expt x power) (first argsL)) accum))))

  ;; set x and calculate the coeffs with x and pow of 0 and accum of 0 becasue its the very start of the function
  (: polyX : Number -> Number)
  (define (polyX x)
     (poly coeffs x 0 0))
  polyX)


(define p2345 (createPolynomial '(2 3 4 5))) 
(test (p2345 0) =>  (+ (* 2 (expt 0 0)) (* 3 (expt 0 1)) (* 4 (expt 0 2)) (* 5 (expt 0 3)))) 
(test (p2345 4) =>     (+ (* 2 (expt 4 0)) (* 3 (expt 4 1)) (* 4 (expt 4 2)) (* 5 (expt 4 3)))) 
(test (p2345 11) => (+ (* 2 (expt 11 0)) (* 3 (expt 11 1)) (* 4 (expt 11 2)) (* 5 (expt 11 3)))) 
 
 
(define p536 (createPolynomial '(5 3 6))) 
(test (p536 11) => (+ (* 5 (expt 11 0)) (* 3 (expt 11 1)) (* 6 
(expt 11 2)))) 
 
(define p_0 (createPolynomial '())) 
(test (p_0 4) => 0)

(define p5-36 (createPolynomial '(5 -3 6))) 
(test (p5-36 11) => (+ (* 5 (expt 11 0)) (* -3 (expt 11 1)) (* 6 
(expt 11 2))))

(define p5-3-6 (createPolynomial '(5 -3 6))) 
(test (p5-3-6 11) => 698)


#|    3.b.1
   The grammar:
     
     <PLANG> ::={{poly <AEs>}{<AEs>}}
     <AEs>   ::= <AE>
             | <AE><AEs>     
     <AE>    ::=<num>
             | {+ <AE> <AE> }
             | {- <AE> <AE> }
             | {* <AE> <AE> }
             | {/ <AE> <AE> }
|#
;;        3.b.2

(define-type PLANG 
    [Poly (Listof AE) (Listof AE)]) 
(define-type AE 
    [Num  Number] 
    [Add  AE AE] 
    [Sub  AE AE] 
    [Mul  AE AE] 
    [Div  AE AE])

(: parse-sexpr : Sexpr -> AE) 
  ;; to convert s-expressions into AEs 
  (define (parse-sexpr sexpr) 
    (match sexpr 
      [(number: n)    (Num n)] 
      [(list '+ lhs rhs) (Add (parse-sexpr lhs)(parse-sexpr rhs))] 
      [(list '- lhs rhs) (Sub (parse-sexpr lhs)(parse-sexpr rhs))] 
      [(list '* lhs rhs) (Mul (parse-sexpr lhs)(parse-sexpr rhs))] 
      [(list '/ lhs rhs) (Div (parse-sexpr lhs)(parse-sexpr rhs))] 
      [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))


(test (parse-sexpr 5) => (Num 5))
(test (parse-sexpr '(+ 3 4)) => (Add (Num 3) (Num 4)))
(test (parse-sexpr '(- 10 5)) => (Sub (Num 10) (Num 5)))
(test (parse-sexpr '(* 2 3)) => (Mul (Num 2) (Num 3)))
(test (parse-sexpr '(/ 8 2)) => (Div (Num 8) (Num 2)))
(test (parse-sexpr '(* (+ 1 2) (- 10 3))) => (Mul (Add (Num 1) (Num 2)) (Sub (Num 10) (Num 3))))
   

(: parse : String -> PLANG) 
  ;; parses a string containing a PLANG expression to a PLANG AST
(define (parse str) 
    (let ([code (string->sexpr str)]) 
         (match code
           ;; case of emtpy poly list
            [(list (list 'poly) (list lst etc)) (error 'parse "at least one coefficient is                        required in ~s" code)]
           ;; case of emtpy points list
           [(list (list 'poly x rest) '() ) (error 'parse "at least one point is  
                       required in ~s" code)]
           ;; case of correct syntax, send to poly each time the map of first of list and the map of the second list
            [(list (list 'poly first ...)(list lst2 ...)) (Poly (map parse-sexpr first) (map parse-sexpr lst2))]
           )))


(test (parse "{{poly 1 2 3} {1 2 3}}")  
     => (Poly (list (Num 1) (Num 2) (Num 3))  
              (list (Num 1) (Num 2) (Num 3))))
(test (parse "{{poly 1/2} {1 -2 3}}")  
     => (Poly (list (Num 1/2))  
              (list (Num 1) (Num -2) (Num 3)))) 
(test (parse "{{poly } {1 2} }")  
     =error> "parse: at least one coefficient is                        required in ((poly) (1 2))") 
(test (parse "{{poly 1 2} {} }")       =error> "parse: at least one point is  
                       required in ((poly 1 2) ())")
(test (parse "{{poly +} {0} }") =error> "bad syntax in +")



;;        3.b.3
;; evaluates AE expressions to numbers 
(: eval : AE -> Number)
(define (eval expr) 
    (cases expr 
      [(Num n)  n] 
      [(Add l r) (+ (eval l) (eval r))] 
      [(Sub l r) (- (eval l) (eval r))] 
      [(Mul l r) (* (eval l) (eval r))] 
      [(Div l r) (/ (eval l) (eval r))]))


;; here we map the first list to choose a number, each time to the end of the list,
;; and we pow to the mapping of second list
  (: eval-poly : PLANG -> (Listof Number) ) 
  (define (eval-poly p-expr) 
    (cases p-expr
      [(Poly lst1 lst2)
       (map (createPolynomial (map eval lst1))
            (map eval lst2))]
      )
    )


  (: run : String -> (Listof Number)) 
  ;; evaluate a FLANG program contained in a string 
  (define (run str) 
    (eval-poly (parse str))) 


(test (run "{{poly 1 2 3} {1 2 3}}")  
=> '(6 17 34)) 
(test (run "{{poly 4 2 7} {1 4 9}}")  => '(13 124 589)) 
(test (run "{{poly 1 2 3} {1 2 3}}")   => '(6 17 34)) 
(test (run "{{poly 4/5 } {1/2 2/3 3}}")  
=> '(4/5 4/5 4/5)) 
(test (run "{{poly 2 3} {4}}")  => '(14)) 
(test (run "{{poly 1 1 0} {-1 3 3}}")  => '(0 4 4))  
(test (run "{{poly {/ 4 2} {- 4 1}} {{- 8 4}}}") 
=> '(14)) 
(test (run "{{poly {+ 0 1} 1 {* 0 9}} {{- 4 5} 3 
{/ 27 9}}}") 
=> '(0 4 4)) 



