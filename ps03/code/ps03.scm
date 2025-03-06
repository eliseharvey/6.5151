#|
Elise Harvey
kerb: erharvey
6.5151 ps03
|#


(load "~/Desktop/6.5151/sdf/manager/load.scm")
(manage 'new 'generic-procedures)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DELETE ME ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(manage 'new 'combining-arithmetics)
(define (vector-element-wise element-procedure)
  (lambda vecs ; Note: this takes multiple vectors
    (ensure-vector-lengths-match vecs)
    (apply vector-map element-procedure vecs)))

(define (ensure-vector-lengths-match vecs)
  (let ((first-vec-length (vector-length (car vecs))))
    (if (any (lambda (v)
               (not (n:= (vector-length v)
                         first-vec-length)))
             vecs)
        (error "Vector dimension mismatch: " vecs))))
(define (vector-addition-maker +)
  (define (vector-addition vec1 vec2)
    (ensure-vector-lengths-match (list vec1 vec2))
    (list->vector
     (map + (vector->list vec1) (vector->list vec2))))
  vector-addition)
(define (vector-negation-maker *)  
  (define (vector-negate vec)  
    ((vector-element-wise (lambda (x) (* x -1))) vec))  
  vector-negate)
(define (vector-subtraction-maker + negate)  
  (define (vector-subtract vec1 vec2)  
    (+ vec1 (negate vec2)))  
  vector-subtract)
(define (vector-dot-product-maker + *)
  (define (vector-dot-product vec1 vec2)
    (ensure-vector-lengths-match (list vec1 vec2))
    (apply + (map * (vector->list vec1) (vector->list vec2))))
  vector-dot-product)
(define (vector-magnitude-maker + * sqrt)
  (let ((vector-dot-product (vector-dot-product-maker + *)))
    (define (vector-magnitude v) (sqrt (vector-dot-product v v)))
    vector-magnitude))
(define (vector-scalar-multiplication-maker-left *)
  (define (vector-scalar-multiply-left scalar vec)
    (list->vector (map (lambda (x) (* scalar x)) (vector->list vec))))
  vector-scalar-multiply-left)
(define (vector-scalar-multiplication-maker-right *) 
  (define (vector-scalar-multiply-right vec scalar) 
    (list->vector (map (lambda (x) (* scalar x)) (vector->list vec)))) 
  vector-scalar-multiply-right)
(define (vec-or-num? x)
  (or (vector? x) (number? x)))
(define (vector-extender base-arithmetic)
  (make-arithmetic 'vector vec-or-num? (list base-arithmetic)
    (lambda (name base-constant)
      base-constant)
    (let ((base-predicate
	   (arithmetic-domain-predicate base-arithmetic)))
      (lambda (operator base-operation)
	(simple-operation
	  operator
	  vec-or-num?
	  (case operator
	    ((+) (lambda (x y) (v:+ x y)))
	    ((-) (lambda (x y) (v:- x y)))
	    ((*) (lambda (x y)
                   (cond
                    ((and (vector? x) (vector? y)) (v:dot x y)) ; both vectors -> use dot
                    ((and (vector? x) (number? y)) (v:mult-right x y)) ; scalar on right -> mult right
                    ((and (number? x) (vector? y)) (v:mult-left x y)) ; scalar on left -> mult right
                    (else (error "Unsupported multiplication")))))
	    ((negate) (lambda (x) (v:negate x)))
	    ((magnitude) (lambda (x) (v:magnitude x)))
	    (else
	     (lambda args
	       (error "Operator undefined in Vector" operator)))))))))
(register-predicate! vec-or-num? 'vector)
(define vector-arithmetic
  (extend-arithmetic vector-extender numeric-arithmetic))
(install-arithmetic! vector-arithmetic)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DELETE ME ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
