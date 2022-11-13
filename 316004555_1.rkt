#lang pl
#|
1.1
i created a function that recive a list of list and merge them all to
a big one list.
my main difficulty was to write the syntax in the right way.
i get help from a frind how to write list of list in the correct way,
take me about an hour after many of variation of declaration.
|#
(: open-list : (Listof (Listof Number)) -> (Listof Number))
(define (open-list lst)
 (cond [(null? lst) null]
       [(= (length lst) 1) (first lst)]
       [else (append (first lst) (open-list(rest lst)))]
  )
 )

(test (open-list '((1 2 3) (2 3 3 4) (9 2 -1) (233 11 90)))
       => '(1 2 3 2 3 3 4 9 2 -1 233 11 90))

(test (open-list '(() (-4048) ()))
       => '(-4048))

(test (open-list '(() () ()))
       => '())

(test (open-list '((2)))
       => '(2))


#|
1.2
here i used 1.1 function, if i cant use it :
so step one is to converte list of list to a list
step 2 is to get by help function the minimal and maximal value
step 3 is to pair and return the returned values
that represent min and max.
took me about an hour
|#

(: myMin : (Listof Number) -> Number)
(define (myMin lst)
    (cond [(=(length lst)0) +inf.0]
    [else (min(first lst)(myMin(rest lst)))])
  )

(: myMax : (Listof Number) -> Number)
(define (myMax lst)
    (cond [(=(length lst)0) -inf.0]
    [else (max(first lst)(myMax(rest lst)))])
  )

(: min&max : (Listof (Listof Number)) -> (Listof Number))
(define (min&max lst)
  (cond [(< (length (open-list lst)) 1) '()]
        [else
         (list (myMin(open-list lst)) (myMax(open-list lst)))]
  )
 )

(test (min&max '((1 2 3) (2 3 3 4) (9 2 -1) (233 11 90)))
             => '(-1.0 233.0))

(test (min&max '((-1) (-1) (-1) ()))
             => '(-1.0 -1.0))

(test (min&max '((6)))
             => '(6.0 6.0))


(test (min&max '(()))
             => '())



#|
1.3
like 2, i used apply to use an function
on en entire list to return the max and the min
so apply used with min and max func on lst
took me about 5 minutes
|#

(: min&max_apply : (Listof (Listof Number)) -> (Listof Number))
(define (min&max_apply lst)
    (cond [(< (length (open-list lst)) 1) '()]
        [else
  (list (apply min (open-list lst)) (apply max (open-list lst)))]
  )
 )

(test (min&max_apply '((1 2 3) (2 3 3 4) (9 2 -1) (233 11 90)))
             => '(-1 233))

(test (min&max_apply '((-1) (-1) (-1) ()))
             => '(-1 -1))

(test (min&max_apply '((6)))
             => '(6 6))


(test (min&max_apply '(()))
             => '())


#|
2.1
create a class table
for now its an empty class with minimal constractor
|#

(define-type Table
  [EmptyTbl]
  )

(test (EmptyTbl) => (EmptyTbl))

#|
2.2
define add function to add data
to an exist table
|#

#|
(: Add : Table Symbol String -> Table)
     (define (Add table symbol string)
(EmptyTbl table symbol string))
|#