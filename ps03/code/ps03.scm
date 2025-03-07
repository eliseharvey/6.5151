#|
Elise Harvey
kerb: erharvey
6.5151 ps03
|#


(load "~/Desktop/6.5151/sdf/manager/load.scm")
(manage 'new 'generic-procedures)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;         PROBLEM 3.4      ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (a)

(((* 3
     (lambda (x) (lambda (y) (+ x y)))
     (lambda (x) (lambda (y) (vector y x))))
  'a)
 4)
(* (* 3 (+ 'a 4)) #(4 'a))

#|
In section 3.1, the combinator-based arithmetic had to specify how to handle the different arguments. We would have to make sure to define ways to handle each case (different varaitions of arguments that could be received).

In general, using combinators is difficult as our system may not be very flexible depending on what order it was implented.
|#


;;; (b)

((magnitude unit-circle) 'a)
((magnitude (vector sin cos)) 'a)

#|
In the previous problem set, I ran into errors where the system struggled with func-before-vec but it looked like vec-before-func could work. Additionally, my system had not worked with symbols so it had errors there.

With this in mind, I think this *could* work if the order was changed and a symbolic extender (ahndler?) was included. 
|#


;;; (c)

#|
A way to make this work is to write a way that applies the functions in the vector one at a time and returns a vector of the output. This would be easy to write, but I think would be an issue when using it with our arithemtic. I feel like this would be "hardcoding" this case and could create issues down the line. I don't know if there is a good/generic way of handling this case that I would feel comfortable actually using.
|#
;; Note: for similpicity, I am NOT over-writing vector and instead saying vectorr
(define (vectorr . elements)
  (lambda (x)
    (apply vector (map (lambda (f) (if (procedure? f) (f x) f)) elements))))
((vectorr cos sin) 3) ; Value: #(-.9899924966004454 .1411200080598672)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;         PROBLEM 3.6      ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; (a)

#|
There are a few things to consider here. First, the order of implementation. It will be easiest to do addition and scalar multiplicaton. Then I can use these two to create negation and subtraction. Lastly, I will tackle matrix multiplication.

From here, I will also need some dimensional checks. For addition and subtraction, the dimensions need to match *exactly*. For scalar multiplication, it does not matter. Then with the matrix multipliation, we need the matrices to be axn and nxb for the output to be axb.
|#

;; Representation is vector of row vectors:
(define example-123456-matrix #(#(1 2 3) #(4 5 6))) ; 2 x 3 matrix
(display example-123456-matrix)

;; getting dimensions of matrix
(define (matrix? obj)
  (and (vector? obj)
       (or (= (vector-length obj) 0)
           (and (vector? (vector-ref obj 0))
                (let ((num-cols (vector-length (vector-ref obj 0))))
                  (every (lambda (row)
                           (and (vector? row)
                                (= (vector-length row) num-cols)))
                         (vector->list obj)))))))
(matrix? #(1 2 3)) ; Value: #f
(matrix? example-123456-matrix) ; Value: #t

(define (m:dims obj) ; if a matrix, will return dimensions (n m) for a nxm matrix
  (if (matrix? obj)
      (let ((n (vector-length obj))
            (m (if (> (vector-length obj) 0)
                   (vector-length (vector-ref obj 0))
                   0)))
        (cons n m))
      (error "Non-matrix passed into matrix-dims!")))
(m:dims #(1 2 3)) ; Non-matrix passed into matrix-dims!
(m:dims example-123456-matrix) ; Value: (2 . 3) 

;; addition, scalar mult, negation, and subtraction
(define (m:+ A B)
  (cond
    ((and (matrix? A) (matrix? B)) ; if both matrices, need to add (only works if same dims)
     (let ((dimA (m:dims A))
           (dimB (m:dims B)))
       (if (equal? dimA dimB)
           (vector-map (lambda (rowA rowB)
                         (vector-map + rowA rowB))
                       A B)
           (error "Matrix dimensions do not match in addition!" dimA dimB))))
    ((and (matrix? A) (number? B)) ; if one is a number, then we add the number element wise to the matrix
     (vector-map (lambda (row)
                   (vector-map (lambda (x) (+ x B)) row))
                 A))
    ((and (matrix? B) (number? A))
     (vector-map (lambda (row)
                   (vector-map (lambda (x) (+ x A)) row))
                 B))
    (else (error "Not a matrix passed into addition!"))))
(define example-111111-matrix #(#(1 1 1) #(1 1 1)))
(m:+ example-111111-matrix example-123456-matrix) ; Value: #(#(2 3 4) #(5 6 7))
(m:+ example-123456-matrix #(#(1 2) #(3 4))) ; Matrix dimensions do not match for addition! (2 . 3) (2 . 2)
(m:+ example-123456-matrix #(1 2)) ; Not a matrix passed into addition!

(define (matrix-scalar-mul A B)
  (cond
    ((and (matrix? A) (number? B))
     (vector-map (lambda (row)
                   (vector-map (lambda (x) (* x B)) row))
                 A))
    ((and (number? A) (matrix? B))
     (vector-map (lambda (row)
                   (vector-map (lambda (x) (* A x)) row))
                 B))
    (else (error "Not a matrix passed into scalar mult!"))))
(matrix-scalar-mul example-123456-matrix 2) ; Value: #(#(2 4 6) #(8 10 12))
(matrix-scalar-mul 10 example-111111-matrix) ; Value: #(#(10 10 10) #(10 10 10))
(matrix-scalar-mul 'a example-111111-matrix) ; Not a matrix passed into scalar mult!

(define (m:negate A)
  (if (matrix? A) ; including this in case I can get operation union to work this time and it tries to check
      (matrix-scalar-mul -1 A)
      (error "Not a matrix passed into negate!")))
(m:negate example-123456-matrix) ; Value: #(#(-1 -2 -3) #(-4 -5 -6))
(m:negate 'a) ; Not a matrix passed into negate! 

(define (m:- A B)
  (m:+ A (m:negate B)))
(m:- example-123456-matrix example-111111-matrix) ; Value: #(#(0 1 2) #(3 4 5))

;; multiplication
(define (transpose matrix)
  (if (matrix? matrix)
      (let* ((rows (vector-length matrix))
             (cols (vector-length (vector-ref matrix 0)))
             (new-matrix (make-vector cols)))
        (do ((j 0 (+ j 1)))
            ((= j cols) new-matrix)
          (let ((new-row (make-vector rows)))
            (do ((i 0 (+ i 1)))
                ((= i rows))
              (vector-set! new-row i (vector-ref (vector-ref matrix i) j)))
            (vector-set! new-matrix j new-row))))
      (error "Not a matrix passed in transpose!")))

(define (m:* A B)
  (if (and (matrix? A) (matrix? B))
      (let* ((dimsA (m:dims A)) ; A is axn
             (dimsB (m:dims B)) ; B is mxb
             (n (cdr dimsA))
             (m (car dimsB)))
        (if (not (= n m)) ; will not work if these are not the same
            (error "Matrix dimensions do not match for multiplication!")
            (let ((B-transposed (transpose B)))
              (vector-map 
               (lambda (rowA)
                 (vector-map 
                  (lambda (colB)
                    (apply + (map * (vector->list rowA) (vector->list colB))))
                  B-transposed))
               A))))
      (error "Not a matrix passed into matrix multiplication!")))


(m:* example-123456-matrix example-111111-matrix) ; Matrix dimensions do not match for multiplication!
(m:* example-123456-matrix #(#(1 1) #(1 1) #(1 1))) ; Value: #(#(6 6) #(15 15)) (yay!)
(m:* 2 example-123456-matrix) ; Not a matrix passed into matrix multiplication!


;;; (b)

#|
The matrix calculations are basically just ways to process integer math in a different format. Tobe able to process symbols in our matrices, we need to enable a symbolic arithmetic. This is simple by extended the numeric arithmetic to handle symbolics.
|#

(define (symbolic? object)
  (or (symbol? object)
      (pair? object)))
(register-predicate! symbolic? 'symbolic)
(define (symbolic-extender base-arithmetic)
  (make-arithmetic 'symbolic symbolic? (list base-arithmetic)
    (lambda (name base-constant)
      base-constant)
    (let ((base-predicate
           (arithmetic-domain-predicate  base-arithmetic)))
      (lambda (operator base-operation)
        (make-operation operator
                        (any-arg (operator-arity operator)
                                 symbolic?
                                 base-predicate)
                        (lambda args (cons operator args)))))))
(define symbolic-arithmetic
  (extend-arithmetic symbolic-extender numeric-arithmetic))
(install-arithmetic! symbolic-arithmetic)

;; try with symbolics
(m:+ example-123456-matrix #(#(a b c) #(d e f))) ; Value: #(#((+ 1 a) (+ 2 b) (+ 3 c)) #((+ 4 d) (+ 5 e) (+ 6 f)))
(define symbolic-transposed (transpose #(#(a b c) #(d e f))))
(m:* example-111111-matrix symbolic-transposed) ; Value: #(#((+ (+ (* 1 a) (* 1 b)) (* 1 c)) (+ (+ (* 1 d) (* 1 e)) (* 1 f))) #((+ (+ (* 1 a) (* 1 b)) (* 1 c)) (+ (+ (* 1 d) (* 1 e)) (* 1 f))))


;;; (c)

#|
If the matrix is dense (not zeroes), then computing the inverse will take several steps, which includes the determinatn. Computing the determinant is almost a recursive process where you need to comput the nxn, the (n-1)x(n-1), ..., until 2x2. If there are no zeroes, we have no terms cancelled out which leads to a hugely complex computation, on the factorial order.  
|#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;         PROBLEM 3.8      ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
For this problem, our goal is to use currying to write a partial derivative. The idea behind this is to "curry" the function so that we can fix all values except for the one to differentiate. This one value to differentate will be the single argument to our curried function (in which we can then compute derivative).

To understand this, let's say we have a function with variables x, y, and z. We want to differentiate with respect to x, we will curry y and z:

(define (f x y z) ... ) |-> (define (f-curry-y x z) lambda (y) ... ) |-> (define (f-curry-yz x) lambda (z) f-curry-y x z)

With this, we curry the function until the only argument passed in the procedure returned is the one we want to differentiate with respect to. We can use partial and apply the currying concept to make it accept one argument.
|#

;; use derivative and other functions defined in SDF
(load "Desktop/6.5151/sdf/automatic-differentiation/derivatives.scm")

(define ((curry-partial-derivative i) f) ; following syntax from (partial i) in SDF codebase, i is the index of the args that we are computing the partial for 
  (lambda args
    (let ((partial-fi ((partial i) f)))
      (lambda (xi) ; this is the currying, we are returning a procedure with a preset argument
        (apply partial-fi (append args (list xi)))))))

;; test with f(x,y) = y^3 + x^4
(define (f x y)
  (+ (expt x 4) (expt y 3)))
(define dfdx ((curry-partial-derivative 0) f))


#|
I am struggling to get this part to work (actually working with the partial derivatives). It has something to do with how I am using partial and then am working with a differential object from derivatives.scm in SDF. In the interest of time, I will get this working if I have time, but for now will need to leave this as is.
|#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;         PROBLEM 3.14     ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-stormer-counts)
  (define (F t x) (- x))
  (define numeric-s0
    (make-initial-history 0 .01 (sin 0) (sin -.01) (sin -.02)))
  (with-predicate-counts
   (lambda ()
     (x 0 ((evolver F 'h stormer-2) numeric-s0 1)))))

(define full-generic-arithmetic
  (let ((g (make-generic-arithmetic make-simple-dispatch-store)))
    (add-to-generic-arithmetic! g numeric-arithmetic)
    (extend-generic-arithmetic! g function-extender)
    (add-to-generic-arithmetic! g
                                (symbolic-extender numeric-arithmetic))
    g))
(install-arithmetic! full-generic-arithmetic)

(define trie-full-generic-arithmetic
  (let ((g (make-generic-arithmetic make-trie-dispatch-store)))
    (add-to-generic-arithmetic! g numeric-arithmetic)
    (extend-generic-arithmetic! g function-extender)
    (add-to-generic-arithmetic! g
                                (symbolic-extender numeric-arithmetic))
    g))
(install-arithmetic! trie-full-generic-arithmetic)


(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))
(fib 20)

#|
The trie only improves performance calls if there are a lot of the same rules in the beginning. This is because that would optimize the tree structure to determine which branch and leaf to go down. If there are a lot of initial simialrities, the trie helps us discern which rules.

If there are a lot of different initial rules, we are basically creating an entirely new branch each time and we won't actually be *better*. Basically, we will still need to make initial comparisons to determine whcih path to traverse. Thus, the strucutre is important if we actually want to avoid redundant calls. 
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;         PROBLEM 3.15     ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "Desktop/6.5151/sdf/common/stormer2.scm")
(load "Desktop/6.5151/sdf/efficient-generic-procedures/microbench.scm")

(define a-cached-dispatch-store
  (cache-wrapped-dispatch-store (make-trie-dispatch-store)
                                implementation-type-name))
(define (cache-wrapped-dispatch-store dispatch-store get-key)
  (let ((get-handler
         (simple-list-memoizer
          eqv?
          (lambda (args) (map get-key args))
          (dispatch-store ’get-handler))))
    (lambda (message)
      (case message
        ((get-handler) get-handler)
        (else (dispatch-store message))))))

;; generic
(with-predicate-counts (lambda () (fib 20))) ; (164179 symbolic), (54727 any-object), (164179 number), (164179 function)
(test-stormer-counts) ; (8 any-object), (30 number), (40 symbolic), (25 function) 

;; trie
(with-predicate-counts (lambda () (fib 20))) ; (109453 symbolic) LOWER, (54727 any-object) SAME, (109453 number) LOWER, (109453 function) LOWER
(test-stormer-counts) ; (10 any-object) HIGHER, (23 number) LOWER, (28 symbolic) LOWER, (22 function) LOWER

#|
For (fib 20), we see that trie is better with 164,178 less predicate counts. Similarly, we have for the test-stormed, trie is better by 20 counts.
|#
