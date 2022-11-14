#lang pl
#|
Task :

Input: 
Output:
Problems I encountered: 
|#

#|
Task 1.1:
In task 1.1 we were asked to receive a list of lists of type Number and return
the concatenation of the lists according to the order we received them.

Input: list of lists of type Number
Output: one list that contains a concatenation of all lists
Problems I encountered: closing parentheses, due to the fact that only in the first assignment I experimented with this programming language
|#
(: open-list : (Listof(Listof Number)) -> (Listof Number))
(define (open-list lst)
  (cond [(null? lst) null]
        [else (append (first lst) (open-list (rest lst)))]
  ))
(test (open-list '(() () () ())) => '())
(test (open-list '((1) (2) (3))) => '(1 2 3))
(test (open-list '(() (1) () (2) () (3))) => '(1 2 3))
(test (open-list '((1 1) (1) () (2) () (3))) => '(1 1 1 2 3))
(test (open-list '((1 2 3) (2 3 3 4) (9 2 -1) (233 11 90))) => '(1 2 3 2 3 3 4 9 2 -1 233 11 90)) ;I took this test from the instructions of the assignment

#|
Task 1.2:
In task 1.2 we were asked to get a list of lists of number types and to return a list that includes 2 numbers that contains the maximum and minimum in all lists.
We used the function from Task 1.1 to merge all the lists and find the requested numbers within the merged list.

We created 2 first helper function:
minimum Gets a list of numbers and returns the minimum number among them
maximum: Gets a list of numbers and returns the maximum number among them

--Note: We created tests for both the helper functions and the task function--

Input: list of lists of type Number
Output: A list that includes 2 numbers: maximum and minimum - from the union of all lists
Problems I encountered: -
|#
(: minimum : (Listof Number) -> Number)
(define (minimum lst)
  (cond [(null? lst) +inf.0]
        [else (min (first lst) (minimum (rest lst)))]
  ))
(test (minimum '(1 2 3 4)) => 1.0)
(test (minimum '(1/2 1/4)) => 0.25)
(test (minimum '(1/2 2 3 4)) => 0.5)
(test (minimum '(1 0.5 300 -10)) => -10.0)


(: maximum : (Listof Number) -> Number)
(define (maximum lst)
  (cond [(null? lst) -inf.0]
        [else (max (first lst) (maximum (rest lst)))]
  ))
(test (maximum '(1 2 3 4)) => 4.0)
(test (maximum '(1/2 1/4)) => 0.5)
(test (maximum '(1/2 2 3 4)) => 4.0)
(test (maximum '(1 0.5 300 -10)) => 300.0)

(: min&max : (Listof(Listof Number)) -> (Listof Number))
(define (min&max lst)
  (cond [(null? lst) null]
        [else (list(minimum(open-list lst)) (maximum(open-list lst)))]
  ))

(test (min&max '((1) (1) (1))) => '(1.0 1.0))       ;because of all the numbers are the same the min and the max should be the same number
(test (min&max '((1) (2) (3))) => '(1.0 3.0))       ;a test that includes an empty list
(test (min&max '(() (1) () ())) => '(1.0 1.0))
(test (min&max '(() () () ())) => '(+inf.0 -inf.0)) ;for testing the auxiliary functions
(test (min&max '(() (1) () (2) () (3))) => '(1.0 3.0))
(test (min&max '((1 1) (1) () (2) () (3))) => '(1.0 3.0))
(test (min&max '((1 2 3) (2 3 3 4) (9 2 -1) (233 11 90))) => '(-1.0 233.0)) ;I took this test from the instructions of the assignment
