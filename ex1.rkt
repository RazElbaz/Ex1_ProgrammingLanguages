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
Time: 40 minutes
|#
(: open-list : (Listof(Listof Number)) -> (Listof Number))
(define (open-list lst)
  (cond [(null? lst) null]
        [else (append (first lst) (open-list (rest lst)))]
  ))
(test (open-list null) =>  null)
(test (open-list '(() () () ())) => '())
(test (open-list '((1) (2) (3))) => '(1 2 3))
(test (open-list '(() (1) () (2) () (3))) => '(1 2 3))
(test (open-list '((1 1) (1) () (2) () (3))) => '(1 1 1 2 3))
(test (open-list '((1 2 3) (2 3 3 4) (9 2 -1) (233 11 90))) => '(1 2 3 2 3 3 4 9 2 -1 233 11 90)) ;I took this test from the instructions of the assignment

#|
Task 1.2:
In task 1.2 we were asked to get a list of lists of number types and to return a list that includes 2 numbers that contains the maximum and minimum in all lists.
We used the function from Task 1.1 to merge all the lists and find the requested numbers within the merged list.

First we created 2 helper functions:
minimum Gets a list of numbers and returns the minimum number among them
maximum: Gets a list of numbers and returns the maximum number among them

--Note: We created tests for both the helper functions and the task function--

Input: list of lists of type Number
Output: A list that includes 2 numbers: maximum and minimum - from the union of all lists
Problems I encountered: -
Time: 20 minutes
|#
(: minimum : (Listof Number) -> Number)
(define (minimum lst)
  (cond [(null? lst) +inf.0]
        [else (min (first lst) (minimum (rest lst)))]
  ))
(test (minimum null) =>  +inf.0)
(test (minimum '(1 2 3 4)) => 1.0)
(test (minimum '(1/2 1/4)) => 0.25)
(test (minimum '(1/2 2 3 4)) => 0.5)
(test (minimum '(1 0.5 300 -10)) => -10.0)

(: maximum : (Listof Number) -> Number)
(define (maximum lst)
  (cond [(null? lst) -inf.0] ;Using Hint: You can use the default min/max values Values like +inf.0 / -inf.0 which refers to ±∞
        [else (max (first lst) (maximum (rest lst)))]
  ))
(test (maximum null) =>  -inf.0)
(test (maximum '(1 2 3 4)) => 4.0)
(test (maximum '(1/2 1/4)) => 0.5)
(test (maximum '(1/2 2 3 4)) => 4.0)
(test (maximum '(1 0.5 300 -10)) => 300.0)

(: min&max : (Listof(Listof Number)) -> (Listof Number))
(define (min&max lst)
  (cond [(null? lst) null]
        [else (list(minimum(open-list lst)) (maximum(open-list lst)))]
  ))
(test (min&max null) => null)
(test (min&max '((1) (1) (1))) => '(1.0 1.0))       ;because of all the numbers are the same the min and the max should be the same number
(test (min&max '((1) (2) (3))) => '(1.0 3.0))       ;a test that includes an empty list
(test (min&max '(() ())) => '(+inf.0 -inf.0))
(test (min&max '(() (1) () ())) => '(1.0 1.0))
(test (min&max '(() () () ())) => '(+inf.0 -inf.0)) ;for testing the auxiliary functions
(test (min&max '(() (1) () (2) () (3))) => '(1.0 3.0))
(test (min&max '((1 1) (1) () (2) () (3))) => '(1.0 3.0))
(test (min&max '((1 2 3) (2 3 3 4) (9 2 -1) (233 11 90))) => '(-1.0 233.0)) ;I took this test from the instructions of the assignment

#|
Task 1.3:
In task 1.3 we were asked to perform the same task as 1.2 only using the apply function of Racket
--Note: The helper functions we created in task 1.2 are not needed--

Input: list of lists of type Number
Output: A list that includes 2 numbers: maximum and minimum - from the union of all lists
Problems I encountered: Lack of familiarity with the apply function -> I learned and understood how to use it
Time: 1 hour and 10 minutes
|#

(: min&max_apply : (Listof(Listof Number)) -> (Listof Number))
(define ( min&max_apply lst)
  (cond [(null? lst) null]
        [(null? (open-list lst)) null]
        [else (list(apply min(open-list lst)) (apply max(open-list lst)))]
  ))
(test (min&max_apply null) => null) ;check the first condition
(test (min&max_apply '(() ())) => null) ;check the second condition
(test (min&max_apply '(())) => null) ;check the second condition
(test (min&max_apply '((1))) => '(1 1))
(test (min&max_apply '((1.0))) => '(1.0 1.0)) ;If one number is of type FLOAT then the new list should return with numbers of that type
(test (min&max_apply '((1) (1) (1))) => '(1 1)) ;because of all the numbers are the same the min and the max should be the same number
(test (min&max_apply '((1) (1) (1.0))) => '(1.0 1.0)) ;If one number is of type FLOAT then the new list should return with numbers of that type
(test (min&max_apply '((1) (2) (3))) => '(1 3)) ;a test that includes an empty list
(test (min&max_apply '(() (1) () ())) => '(1 1))
(test (min&max_apply '(() (1) () (2) () (3))) => '(1 3))
(test (min&max_apply '((1 1) (1) () (2) () (3))) => '(1 3))
(test (min&max_apply '((1 2 3) (2 3 3 4) (9 2 -1) (233 11 90))) => '(-1 233)) ;I took this test from the instructions of the assignment


#|
Task 2:
In this task we will have to define a new type named Table, create constructors and functions for it.
---------------------------------------------------------------------------------------------------------------------------------------------
Task 2.1:
In task 2.1 we were asked to defining a Table object - this will act as a Table data structure and to built an empty constructor.
Each element in the table will be keyed (indexed) with a symbol.
I built an empty constructor and a constructor that accepts parameters (key, value, table).
Input:-
Output: -
Problems I encountered: -
---------------------------------------------------------------------------------------------------------------------------------------------
Task 2.2:
In task 2.2 we had to implement the Add operation but this too should be a variant of thedata type so I built a constructor that accepts
parameters (key, value, table).

Input: a symbol (key), a string (value), and an existing table
Output: extended table
Problems I encountered: At first I only created an empty constructor, I created a function called Add and after reading it I realized that I
                        needed to create a constructor that accepts parameters - and so I did.
---------------------------------------------------------------------------------------------------------------------------------------------
Time: 2 hours
|#

(define-type Table
  [EmptyTbl] ; Task 2.1: empty constructor
  [Add Symbol String Table] ; Task 2.2: A constructor that accepts parameters
  )

(test (EmptyTbl) => (EmptyTbl))  ;I took this test from the instructions of the assignment
(test (Add 'a "A" (EmptyTbl))=> (Add 'a "A" (EmptyTbl)))
(test (Table? (EmptyTbl)) => #t) ;I used the example from lecture 3 where we tested whether a snake is an animal
(test (Table? (Add 'a "A" (EmptyTbl))) => #t)

#|
Task 2.3:
In task 2.3 we had to implement the search operation search-table - the search operation which should take as input a symbol (key) and a table
and return the first one that the function finds if the key does not appear in the original table, it should return a #f value

Input:
1) Symbol InSymbol - A symbol we need to search for in the Table
2) Table InTable - A table where we need to look for the symbol in it

Output:
String- The first value (string) that matches the key (symbol)
OR
#f- if the key does not appear in the original table

Problems I encountered:I was having trouble trying to recursively access a table object I created.
After repeating exercises and lectures and i realizing that when we use ADD we seem to get the rest of the table and go through the other keys, I was able to solve this task.
This is the task that took me the most time.

Time: 2 hours and 15 minutes
|#


(: search-table : Symbol Table ->(U String #f)) ;I used lecture 3 where we defined a union of returnable variables
(define (search-table InSymbol InTable)
  (cases InTable
    [(EmptyTbl) #f]
    [(Add CurrSymbol CurrString CurrTable) ;CurrSymbol-current Symbol  CurrString-current String CurrTable-current Table
    (if(equal? InSymbol CurrSymbol)CurrString ;If the requested symbol is found
       (search-table InSymbol CurrTable)) ;Otherwise, we will repeat the function recursively
    ]))
;test empty styring-> "" is empty string
(test (search-table 'a (Add 'a "" (Add 'b "B" (Add 'a "A"(EmptyTbl)))))=> "")
;test non empty styring-> " " is not an empty string
(test (search-table 'a (Add 'a " " (Add 'b "B" (Add 'a "A"(EmptyTbl)))))=> " ")
;test false+tests given in the assignment
(test (search-table 'c (Add 'a "AAA" (Add 'b "B" (Add 'a "A"(EmptyTbl)))))=> #f)
(test (search-table 'r (Add 'a "AAA" (Add 'b "B" (Add 'a "A"(EmptyTbl)))))=> #f)
(test (search-table 'a (Add 'r "AAA" (Add 'b "B" (Add 'a "A"(EmptyTbl)))))=> "A")
(test (search-table 'b (Add 'r "AAA" (Add 'b "B" (Add 'a "A"(EmptyTbl)))))=> "B")
;A test of a symbol that is a word and not a single character
(test (search-table 'hello (Add 'hello "HELLO" (Add 'world "WORLD" (Add '! "!"(EmptyTbl)))))=> "HELLO")
(test (search-table 'world (Add 'hello "HELLO" (Add 'world "WORLD" (Add '! "!"(EmptyTbl)))))=> "WORLD")
;A test of a symbol that is not a letter
(test (search-table '! (Add 'hello "HELLO" (Add 'world "WORLD" (Add '! "!"(EmptyTbl)))))=> "!")
(test (search-table '!!!!! (Add 'hello "HELLO" (Add 'world "WORLD" (Add '!!!!! "!"(EmptyTbl)))))=> "!")
(test (search-table '!!!!! (Add 'hello "HELLO" (Add 'world "WORLD" (Add '!!!! "!"(EmptyTbl)))))=> #f) 