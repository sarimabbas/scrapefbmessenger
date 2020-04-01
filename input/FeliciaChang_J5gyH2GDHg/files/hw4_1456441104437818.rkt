#lang racket

(provide 
         entry entry? entry-key entry-value
         e-not e-not? e-not-arg
         e-and e-and? e-and-args
         e-or e-or? e-or-args
         tt tt-vars tt-rows tt?
	 hours
         lookup
         f-not f-and f-or
         boolean-exp?
	 all-vars
	 eval-in-env
	 all-keys
	 truth-table	 	 	 
         classify equivalent?
	 find-exp
         simplify)

; Please do not modify lines above this one.

; ****************************************************************
; CS 201 HW #4  DUE 11:59 pm Wednesday, March 1, 2017
; using the submit command on the Zoo.
; ****************************************************************
; Name: Sarim Abbas
; Email address: sarim.abbas@yale.edu
; ****************************************************************
; ** problem 0 ** (1 easy point)
; Please modify the following definition to reflect the number of
; hours you spent on this assignment.  (Must be non-zero to earn credit.)

(define hours 18)

; ****************************************************************
; Please DO NOT use require or mutators (set! and its relatives)
; in your programs.  Otherwise you may use any Racket constructs.

; You may write auxiliary procedure(s) in addition to
; the one(s) specified in the problems.  Please include
; a comment for each one explaining its input and results.

; Topics:
; Racket: deep recursion on a recursively defined data structure.
; Computer Science: Boolean functions, expressions, environments,
; truth tables, logical equivalence, tautology, contradiction,
; contingency, simplification.

; ****************************************************************
; We define a table as a list of entries,
; where each entry is given by the following structure.

(struct entry (key value) #:transparent)

; Recall that a struct defines a constructor, selector(s), and a type predicate.
; In this case, the constructor is entry, the selectors are
; entry-key and entry-value, and the type predicate is entry?.

; Here are two examples of tables.

(define table1
  (list
   (entry "second" 2)
   (entry "first" 1)
   (entry "fifth" 5)))

(define table2
  (list
   (entry 'x 0)
   (entry 'z 1)
   (entry 'y 1)
   (entry 'z 0)))

; ****************************************************************
; ** problem 1 ** (5 points)
; Write a procedure to deal with tables as follows.

; (lookup key table)

; returns #f if no entry in the table has a key equal? to key
; otherwise, returns the value of the first entry whose key is equal? to key.

; Examples
; (lookup "first" table1) => 1
; (lookup "third" table1) => #f
; (lookup 'z table2) => 1
; ****************************************************************

(define (lookup key table)
  (if (empty? table)
      #f (if (equal? key (entry-key (first table)))
             (entry-value (first table)) (lookup key (rest table)))))

; ****************************************************************
; ** problem 2 ** (5 points)
; Write three procedures to compute Boolean functions as follows.

; (f-not val)
; (f-and lst)
; (f-or lst)

; (f-not val) takes a single Boolean value represented by 0 or 1
; and returns the negation (NOT) of it.

; (f-and lst) takes a list lst of Boolean values represented by 0 or 1
; and returns the conjunction (AND) of them all.  If lst is empty, then
; the value returned should be 1.

; (f-or lst) takes a list lst of Boolean values represented by 0 or 1
; and returns the disjunction (OR) of them all.  If lst is empty, then
; the value returned should be 0.

; Examples
; (f-not 0) => 1
; (f-and '()) => 1
; (f-and '(1 1 0 1)) => 0
; (f-or '()) => 0
; (f-or '(1)) => 1
; ****************************************************************

(define (f-not val)
  (cond
    [(equal? val 1) 0]
    [(equal? val 0) 1]))

(define (f-and lst)
  (if (member 0 lst)
            0 1))
      

(define (f-or lst)
  (if (member 1 lst)
            1 0))

; ****************************************************************
; Our representation of Boolean expressions will use the following
; struct definitions, for the operations of NOT, AND, OR.

(struct e-not (arg) #:transparent)
(struct e-and (args) #:transparent)
(struct e-or (args) #:transparent)

; These define constructors, selectors, and type predicates as follows
; e-not, e-not-arg, e-not?
; e-and, e-and-args, e-and?
; e-or, e-or-args, e-or?

; A Boolean expression is defined recursively as follows.
; 1) the constants 0 and 1 are Boolean expressions
; 2) any identifier is a Boolean expression, where the variable x
; is represented by the symbol 'x
; 3) if <E> is any Boolean expression, its negation (NOT) is represented
; by (e-not <E>).
; 4) if <E1>, ..., <En> are any Boolean expressions, their conjunction (AND)
; is represented by (e-and (list <E1> ... <En>)).  If the list is empty,
; then the AND expression is equivalent to the constant 1.
; 5) if <E1>, ..., <En> are any Boolean expressions, their disjunction (OR)
; is represented by (e-or (list <E1> ... <En>)).  If the list is emptcy,
; then the OR expression is equivalent to the constant 0.

; Some examples of Boolean expressions:

; The expression 0'
(define exp1 (e-not 0))

; The expression (x + y)
(define exp2 (e-or (list 'x 'y)))

; The expression (x * y * z)
(define exp3 (e-and (list 'x 'y 'z)))

; The expression (w * (x' + 0 + y)))
(define exp4 (e-and (list 'w (e-or (list (e-not 'x) 0 'y)))))

; The expression (x + x')
(define exp5 (e-or (list 'x (e-not 'x))))

; ****************************************************************
; ** problem 3 ** (10 points)
; Write a procedure

; (boolean-exp? val)

; (boolean-exp? val) takes an arbitrary Racket value val
; and tests to see whether it is a Boolean expression according
; to the definition above, returning #t if so and #f if not.

; The predicate (symbol? arg) will test whether its argument
; is an identifier.

; Hint: deep recursion on the structure of Boolean expressions.

; Examples
; (boolean-exp? 0) => #t
; (boolean-exp? 2) => #f
; (boolean-exp? exp1) => #t
; (boolean-exp? (e-and (list 'x "hi!"))) => #f
; (boolean-exp? (e-and (list 'x 0 'y))) => #t
; (boolean-exp? (e-or (list 'x (e-and (list 'y #t))))) => #f
; ****************************************************************


(define (boolean-exp? val)
  (if (and (list? val) (not (or (e-type? val)))) ; deal with lists like '(1 2 3 4)
      #f
      (boolean-exp-aux? 0 val)))


; the reason this auxillary procedure had to be created was to use the tracker
; the reason a tracker was needed was to prevent this scenario: (boolean-exp? '()) => #t

(define (boolean-exp-aux? tracker val)  
  (if (list? val) ; is it a list?
      (if (empty? val) ; if so, is the list empty?
          (if (not (equal? tracker 0)) ; check recursion level via tracker to return accordingly 
              #t #f) 
          (and (boolean-exp-aux? (+ 1 tracker) (first val)) (boolean-exp-aux? (+ 1 tracker) (rest val)))) ; if not, check the list
      (if (e-not? val) ; is it an e-not expr?
          (if (list? (e-not-arg val)) ; is its argument a list?
              #f ; if so, return false
              (boolean-exp-aux? (+ 1 tracker) (e-not-arg val))) ; if not, search the arg
          (if (e-or? val) ; if not, is it an e-or expr?
              (if (list? (e-or-args val)) ; are its arguments a list?
                  (boolean-exp-aux? (+ 1 tracker) (e-or-args val)) ; if so, search the args
                  #f) ; if not, return a false
              (if (e-and? val) ; if not, is it an e-and expr?
                  (if (list? (e-and-args val)) ; are its arguments a list?
                      (boolean-exp-aux? (+ 1 tracker) (e-and-args val)) ; if so, search the args
                      #f) ; if not, return a false
                  (if (or (io? val) (symbol? val)) ; if not, is it either a 1/0 or a symbol?
                      #t #f)))))) ; return accordingly

; small auxillary predicate that tests for 1s and 0s
(define (io? val)
  (if (or (equal? val 0) (equal? val 1))
      #t #f))

; anything which is an e-or, e-and or e-not
(define (e-type? bexp)
  (if (or (e-not? bexp) (e-and? bexp) (e-or? bexp))
      #t #f))

; ****************************************************************
; ** problem 4 ** (10 points)
; Write a procedure

; (all-vars bexp)

; that takes a Boolean expression bexp 
; and makes a list containing all the variables
; that occur in bexp.  The list should not contain duplicates,
; and should have the variables in the order of their
; first appearance in bexp (scanning left to right.)

; Hint: deep recursion on the structure of Boolean expressions.

; Note that there is a Racket procedure: remove-duplicates

; Examples

; (all-vars 0) =>'()
; (all-vars 'x) => '(x)
; (all-vars (e-not (e-and (list 'x (e-or (list 'y (e-not 'z) 'x)))))) => '(x y z)
; (all-vars (e-and (list 1 (e-or (list 0 (e-not 'u)))))) => '(u)
; (all-vars (e-and (list (e-or (list 'x 'y)) (e-or (list (e-not 'y) (e-not 'x)))))) => '(x y)
; (all-vars (e-or (list 'c 'b 'a (e-and (list 'a 'b 'c))))) => '(c b a)
;*************************************************

(define (all-vars bexp)  
  (if (list? bexp) ; is it a list?
      (if (empty? bexp) ; if so, is the list empty?
          '()
          (remove-duplicates (flatten (cons (all-vars (first bexp)) (all-vars (rest bexp)))))) ; if not, check the list
      (if (e-not? bexp) ; is it an e-not expr?
          (all-vars (e-not-arg bexp)) ; if so, search the arg
          (if (e-or? bexp) ; if not, is it an e-or expr?
              (all-vars (e-or-args bexp)) ; if so, search the args
              (if (e-and? bexp) ; if not, is it an e-and expr?
                  (all-vars (e-and-args bexp)) ; if so, search the args
                  (if (io? bexp) ; if not, is it a binary digit
                      '() ; if so, return accordingly
                      (if (symbol? bexp) ; if not, is it a symbol?
                          (list bexp) ; if so, return accordingly
                          '()))))))) ; if not, return accordingly
             

; ****************************************************************
; We represent an environment as table each entry of which
; has a key that is a Racket symbol and a value that is 0 or 1,
; which specifies the truth value of that variable in the environment.
; For example:

(define environ1
  (list
   (entry 'x 0) (entry 'y 1) (entry 'z 0)))
  
(define environ2
  (list
   (entry 'u 0) (entry 'x 1) (entry 'w 1) (entry 'y 0) (entry 'z 1)))

; ****************************************************************
; ** problem 5 ** (10 points)
; Write a procedure 

; (eval-in-env bexp env)

; that takes a Boolean expression bexp and an environment env
; (represented as described above) and returns 0 or 1 giving 
; the value of the expression in the environment.

; If the Boolean expression contains variables that do not
; occur in the environment, (eval-in-env bexp env) should
; return the string: "variable unspecified in environment".
; (You may want to check for this condition first.)

; Hint: deep recursion on the structure of Boolean expressions.

; Examples

; (eval-in-env 1 environ1) => 1
; (eval-in-env (e-or (list 0 0 0)) '()) => 0
; (eval-in-env 'x environ1) => 0
; (eval-in-env 'x environ2) => 1
; (eval-in-env (e-not 'z) environ1) => 1
; (eval-in-env (e-or (list 'y (e-not 'x))) environ2) => 0
; (eval-in-env (e-and (list (e-or (list 'y 'x)) (e-or (list 'w 'y)))) environ2) => 1
; (eval-in-env exp5 environ1) => 1
; (eval-in-env (e-and (list 'x 'y 'z)) (list (entry 'x 1) (entry 'z 0))) => "variable unspecified in environment"
; ****************************************************************

(define (eval-in-env bexp env)
  (if (equal? (sanity-check bexp env) #f)
      "variable unspecified in environment"
      (if (list? bexp) ; is it a list?
          (if (empty? bexp) ; if so, is the list empty?
              '()
              (flatten (cons (eval-in-env (first bexp) env) (eval-in-env (rest bexp) env)))) ; if not, check the list
          (if (e-not? bexp) ; is it an e-not expr?
              (f-not (eval-in-env (e-not-arg bexp) env)) ; if so, search the arg
              (if (e-or? bexp) ; if not, is it an e-or expr?
                  (f-or (eval-in-env (e-or-args bexp) env)) ; if so, search the args
                  (if (e-and? bexp) ; if not, is it an e-and expr?
                      (f-and (eval-in-env (e-and-args bexp) env)) ; if so, search the args
                      (if (io? bexp) ; if not, is it a binary digit
                          bexp ; if so, return accordingly
                          (if (symbol? bexp) ; if not, is it a symbol?
                              (lookup bexp env) ; if so, lookup and return accordingly
                              '()))))))))

; multi-lookup uses lookup on a list of variables
(define (multi-lookup lst env)
  (if (empty? lst)
      #t
      (if (equal? (lookup (first lst) env) #f)
          #f
          (multi-lookup (rest lst) env))))
        
; sanity-check runs all-vars on a bexp, then multi-lookup on the resulting list
(define (sanity-check bexp env)
  (let ([lst (all-vars bexp)])
    (if (equal? (multi-lookup lst env) #t)
        #t
        #f)))
        

; ****************************************************************
; We define a truth table as represented by the following struct

(struct tt (vars rows) #:transparent)

; whose fields contain the following
; (1) a (possibly empty) list of n distinct variables, and
; (2) a table containing an entry for each row of the truth table:
; the key of an entry is a list of n 0's and 1's, and the value is the
; Boolean value (0 or 1) of the function on that row of the table.

; Note that the entries in a truth table should be in increasing order of
; their keys, considered as binary numbers.

; Examples of truth tables for the functions given by
; x', (x * y), (a NAND b), (u XOR v)

(define tt-not (tt '(x)
                   (list
                    (entry '(0) 1)
                    (entry '(1) 0))))

(define tt-and (tt '(x y)
                   (list 
                    (entry '(0 0) 0)
                    (entry '(0 1) 0)
                    (entry '(1 0) 0)
                    (entry '(1 1) 1))))
                    
 (define tt-nand (tt '(a b)
                   (list
                    (entry '(0 0) 1)
                    (entry '(0 1) 1)
                    (entry '(1 0) 1)
                    (entry '(1 1) 0))))
  
(define tt-xor (tt '(u v)
                   (list
                    (entry '(0 0) 0)
                    (entry '(0 1) 1)
                    (entry '(1 0) 1)
                    (entry '(1 1) 0))))

; Here is a truth table for a function of three arguments a, b, c.

(define tt-f1 (tt '(a b c)
                  (list
                   (entry '(0 0 0) 0)
                   (entry '(0 0 1) 0)
                   (entry '(0 1 0) 1)
                   (entry '(0 1 1) 1)
                   (entry '(1 0 0) 0)
                   (entry '(1 0 1) 1)
                   (entry '(1 1 0) 0)
                   (entry '(1 1 1) 1))))

; ****************************************************************
; ** problem 6 ** (10 points)
; Write a procedure 

; (all-keys n)

; that takes a non-negative integer n and creates the list of all 
; lists of n 0's or 1's in the *specific order* required for
; a truth table.  In other words, the lists, interpreted as
; binary numbers, should be in increasing order.

; Hint: if a recursive call gives the correct answer
; for (all-keys 2), what needs to happen to it
; to give the correct answer for (all-keys 3)?
; (Compare bit-strings from lecture and all-subsets from hw #2.)

; Use let or let* to avoid recomputing the recursive call!

; Examples
; (all-keys 0) => '(())
; (all-keys 1) => '((0) (1))
; (all-keys 2) => '((0 0) (0 1) (1 0) (1 1))
; (all-keys 3) => '((0 0 0) (0 0 1) (0 1 0) (0 1 1) (1 0 0) (1 0 1) (1 1 0) (1 1 1))
; ****************************************************************

(define (all-keys n)
  (if (= n 0)
      '(())
      (if (= n 1)
      '((0) (1))
      (let ([previous (all-keys (- n 1))])
        (append (put-first 0 previous) (put-first 1 previous))))))
	
; aux procedure to put 1 or 0 at the start of each list of keys
(define (put-first x lsts)
  (map (lambda (lst) (cons x lst)) lsts))

; ****************************************************************
; ** problem 7 ** (10 points)
; Write a procedure

; (truth-table bexp)

; that takes a Boolean expression bexp and returns the truth table for bexp
; where the variables for the table are extracted from bexp using all-vars, 
; and the function value for each row is obtained by evaluating bexp 
; in the corresponding environment.  Notice that all-vars specifies
; the order of variables for the truth table.

; Examples:

;> (truth-table exp1)
;(tt '() (list (entry '() 1)))

;> (truth-table exp2)
;(tt
; '(x y)
; (list (entry '(0 0) 0) (entry '(0 1) 1) (entry '(1 0) 1) (entry '(1 1) 1)))

;>  (truth-table exp4)
;(tt
; '(w x y)
; (list
;  (entry '(0 0 0) 0)
;  (entry '(0 0 1) 0)
;  (entry '(0 1 0) 0)
;  (entry '(0 1 1) 0)
;  (entry '(1 0 0) 1)
;  (entry '(1 0 1) 1)
;  (entry '(1 1 0) 0)
;  (entry '(1 1 1) 1)))
; ****************************************************************

; calls table-maker to create the truth table
(define (truth-table bexp)
  (let ([vars (all-vars bexp)])
    (tt vars (table-maker vars (all-keys (length vars)) bexp))))


; creates a table from a list of variables, a list of keys, and a boolean expression
(define (table-maker vars keys bexp)
  (if (empty? keys) '()
      (cons (entry (first keys) (eval-in-env bexp (environment-maker vars (first keys))))
            (table-maker vars (rest keys) bexp))))


; converts a key to an environment for use with (eval-in-env bexp env)
; takes vars (a list of variables) and a key (a list of binary digits)
(define (environment-maker vars key)
  (if (not (equal? (length vars) (length key)))
      "key and vars list length must be the same"
      (if (or (empty? vars) (empty? key)) '()
          (cons (entry (first vars) (first key)) (environment-maker (rest vars) (rest key))))))

; ****************************************************************
; ** problem 8 ** (10 points)
; Write two procedures

; (classify bexp)
; (equivalent? bexp1 bexp2)

; (classify bexp) takes a Boolean expression and classifies it, returning one of the
; identifiers: 'tautology, 'contradiction, or 'contingent.
; The expression is a tautology if it is true in every environment,
; a contradiction if it is false in every environment, and contingent
; if it is true in some environments and false in some environments.

; (equivalent? bexp1 bexp2) takes two Boolean expressions and returns #t if
; they are logically equivalent, and #f if they are not logically equivalent.
; Two expressions are logically equivalent if, for every environment that
; assigns Boolean values to ALL the variables that appear in either expression,
; the two expressions have the same value.

; For example, the expression 'a is not equivalent to the expression 'b,
; because in the environment (list (entry 'a 0) (entry 'b 1)),
; the first expression takes the value 0, but the second expression takes the value 1.

; These procedures will be tested on expressions with few enough
; variables that generating truth tables WILL BE a feasible approach.

; Examples
; (classify 0) => 'contradiction
; (classify (e-or (list 'x (e-not 'x)))) => 'tautology
; (classify exp2) => 'contingent
; (classify exp3) => 'contingent
; (classify (e-and '())) => 'tautology

; (equivalent? 0 (e-and (list 'a 0))) => #t
; (equivalent? 'a 'b) => #f
; (equivalent? (e-not (e-or (list 'a 'b 'c))) (e-and (list (e-not 'c) (e-not 'b) (e-not 'a)))) => #t
; ****************************************************************

; gathers all outputs of (tt-rows (truth-table bexp)) into a list
(define (output-gather entries)
  (if (empty? entries) '()
      (cons (entry-value (first entries)) (output-gather (rest entries)))))

; check all the outputs of the truth-table in the form '(1 0 1 0 0 ...)
; if ouputs are all 1 => tautology
; if outputs are all 0 => contradiction
; if outputs are a mix => contingent
(define (classify bexp)
  (let ([outputs (output-gather (tt-rows (truth-table bexp)))])
    (if (and (member 0 outputs) (member 1 outputs))
        'contingent
        (if (member 0 outputs)
            'contradiction
            'tautology))))

; two equivalent boolean expressions must satisfy the (XOR)' gate
; bexp1 bexp2 (only if equal bexp1 = bexp2)
; ----- ----- -----------------------------
; 0     0     1
; 0     1     0
; 1     0     0
; 1     1     1

; via sum of products: the expression (bexp1.bexp2) + (bexp1'.bexp2') must always return 1 for bexp1 = bexp2
(define (equivalent? bexp1 bexp2)
  (if
   (equal? (classify (e-or (list (e-and (list bexp1 bexp2)) (e-and (list (e-not bexp1) (e-not bexp2)))))) 'tautology)
              #t #f))
              
; ****************************************************************
; ** problem 9 ** (20 points)
; Write a procedure

; (find-exp tt)

; This procedure takes a truth table
; and returns a Boolean expression 
; for the given truth table.

; You may choose to use the sum-of-products algorithm
; from lecture, or some other method, but it must
; return a Boolean expression with the given truth table.

; Examples
; (find-exp tt-and) => (e-or (list (e-and '(x y))))

; (find-exp tt-nand) => (e-or
;                        (list
;                         (e-and (list (e-not 'a) (e-not 'b)))
;                         (e-and (list (e-not 'a) 'b))
;                         (e-and (list 'a (e-not 'b)))))

; (find-exp tt-xor) =>(e-or (list (e-and (list (e-not 'u) 'v)) (e-and (list 'u (e-not 'v))))

; (find-exp tt-f1) => (e-or
;                      (list
;                       (e-and (list (e-not 'a) 'b (e-not 'c)))
;                       (e-and (list (e-not 'a) 'b 'c))
;                       (e-and (list 'a (e-not 'b) 'c))
;                       (e-and '(a b c))))

; ****************************************************************

(define (find-exp tt)
  (find-exp-aux (tt-vars tt) (tt-rows tt)))

; this auxilliary procedure takes in the truth-table's variable list as well as its entries
; the caveat with this implementation is that the result is inside an extra e-or. But logically, things work.
(define (find-exp-aux var-list entry-list)
  (if (empty? entry-list) ; if reached the end of recursion, return empty list
      '()
      (if (equal? (entry-value (first entry-list)) 1) ; if there is a 1 in the output
          (e-or (flatten (list (e-and (flatten (list (flatten (inputs-to-bexp (entry-key (first entry-list)) var-list))))) (find-exp-aux var-list (rest entry-list)))))
          ; the above:
          ; first take the input of that entry
          ; then send it to inputs-to-bexp to generate a list of boolean expressions
          ; wrap the above - as well as the result of find-exp-aux called on other entries - inside an e-and
          ; wrap the above in an e-or
          (find-exp-aux var-list (rest entry-list))))) ; if the first entry wasn't a match, call the procedure on the rest of the entries


; for a given list of inputs, and a given list of symbols, this procedure creates a list of modified symbols to reflect the inputs
; example: (inputs-to-bexp '(1 0 1) '(x y z)) => '(x (e-not 'y) z)
(define (inputs-to-bexp input-list symbol-list)
  (if (or (empty? symbol-list) (empty? input-list))
      '()
      (if (equal? (first input-list) 1)
          (cons (first symbol-list) (inputs-to-bexp (rest input-list) (rest symbol-list)))
          (cons (e-not (first symbol-list)) (inputs-to-bexp (rest input-list) (rest symbol-list))))))


; problem 9 test:
; (equivalent?
; (find-exp (tt '(v w x) (list (entry '(0 0 0) 1) (entry '(0 0 1) 0) (entry '(0 1 0) 0) (entry '(0 1 1) 0) (entry '(1 0 0) 0) (entry '(1 0 1) 0) (entry '(1 1 0) 0) (entry '(1 1 1) 1))))
; (e-or (list (e-and (list (e-not 'v) (e-not 'w) (e-not 'x))) (e-and (list 'v 'w 'x)))))
; this works 

; ****************************************************************
; ** problem 10 ** (9 points)
; Write a procedure

; (simplify bexp)

; that takes a Boolean expression bexp and returns
; an equivalent Boolean expression that is
; simplified as much as possible using the following rules:

; x + 0 -> x
; 0 + x -> x
; x + 1 -> 1
; 1 + x -> 1
; x * 0 -> 0
; 0 * x -> 0
; x * 1 -> x
; 1 * x -> x
; 0' -> 1
; 1' -> 0
; (x')' -> x

; Also note that
; (e-or '()) should be simplified to 0
; (e-and '()) should be simplified to 1
; (e-or (list bexp)) should be simplified to bexp
; (e-and (list bexp)) should be simplified to bexp

; Examples
; (simplify 0) => 0
; (simplify 'x) => 'x
; (simplify (e-not (e-not 'x))) => 'x
; (simplify (e-not 'y)) => (e-not 'y)
; (simplify (e-or (list 0 (e-and (list 1 (e-not 0) 1))))) => 1
; (simplify (e-and (list 'x 1))) => 'x
; (simplify (e-or (list 0 'z 0))) => 'z
; (simplify (e-or (list (e-and (list 'x 1)) (e-or (list (e-not 1) 'z))))) => (e-or '(x z))
; ****************************************************************

; generates a variable helplist based on a symbol
(define (dynamic-helplist symbol)
  (list
   [entry (e-not (e-not symbol)) symbol]
   [entry (e-not (e-not (e-not symbol))) (e-not symbol)]
   [entry (e-not 1) 0]
   [entry (e-not 0) 1]
   [entry (e-and (list 1 symbol)) symbol]
   [entry (e-and (list symbol 1)) symbol]
   [entry (e-and (list 0 symbol)) 0]
   [entry (e-and (list symbol 0)) 0]
   [entry (e-or (list 1 symbol)) 1]
   [entry (e-or (list symbol 1)) 1]
   [entry (e-or (list 0 symbol)) 0]
   [entry (e-or (list symbol 0)) 0]))

; single variable helplist-search
(define (helplist-search entry helplist)
  (if (empty? helplist)
      entry (if (equivalent? entry (entry-key (first helplist)))
             (entry-value (first helplist))
             (helplist-search entry (rest helplist)))))

; dynamic helplist search given a symbol
; examples:
; (dynamic-search (e-not (e-not 'x)) 'x) => 'x
; (dynamic-search (e-not (e-not 'x)) 'z) => (e-not (e-not 'x))
(define (dynamic-search entry symbol)
  (helplist-search entry (dynamic-helplist symbol)))

; strip an element from the top-level of a list
(define (strip-x x lst)
  (if (list? lst)
      (if (empty? lst)
          '()
          (if (equal? (first lst) x)
              (strip-x x (rest lst))
              (cons (first lst) (strip-x x (rest lst)))))
      lst))

(define (simplify-aux bexp)
  (if (e-type? bexp) ; is it an e-type?
      (if (= (length (all-vars bexp)) 1) ; if so, are the no. of variables inside = 1?
          (dynamic-search bexp (first (all-vars bexp))) ; if so, search the expression against the helplist generated for the symbol inside
          (if (= (length (all-vars bexp)) 0)
              (dynamic-search bexp 'random)
              (if (e-or? bexp) ; if no. of variables inside are more than 1, is it an e-or?
                  (e-or (simplify (strip-x 0 (e-or-args bexp)))) ; if so, create an e-or and simplify its argument
                  (if (e-and? bexp) ; if not, is it an e-and?
                      (e-and (simplify (strip-x 1 (e-and-args bexp)))) ; if so, create an e-and and simplify its argument
                      (if (e-not? bexp) ; if not, is it an e-not?
                          (e-not (simplify (e-not-arg bexp))) ; if so, create an e-not and simplify its argument
                          "this isn't even an e-type bro"))))) ; edge case
      (if (list? bexp) ; if not an e-type, is it a list?
          (if (empty? bexp) ; is the list empty?
              '() ; if so, return a '()
              (flatten (cons (simplify (first bexp)) (simplify (rest bexp))))) ; if not, create a list with simplified insides
          (if (or (io? bexp) (symbol? bexp)) ; if not a list, is it a binary digit or symbol?
              bexp ; if so, return that digit or symbol
              '())))) ; if not, return an empty list

(define (simplify bexp)
  (simplify-aux (simplify-aux bexp)))

              
; **************** end of hw #4 *********************************



