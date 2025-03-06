#|
Elise Harvey
kerb: erharvey
6.5151 ps02
|#

(load "~/Desktop/6.5151/sdf/manager/load.scm")
(manage 'new 'combining-arithmetics)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;         PROBLEM 3.1      ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Q: 3.1 from SDF. Use the Scheme predicate boolean? to make a boolean arithmetic package that can be combined with the arithmetics we have.

(define boolean-arithmetic
  (make-arithmetic 'boolean boolean? '()
                    (lambda (name)
                      (case name
                        ((additive-identity) #f)
                        ((multiplicative-identity) #t)
                        (else (default-object))))
                    (lambda (operator)
                      (let ((procedure
                             (case operator
                               ((+) (lambda (x y) (or x y))) ; x or y
                               ((-) (lambda (x y) (or x (negate y)))) ; x or not y
                               ((*) (lambda (x y) (and x y))) ; x and y
                               ((negate) (lambda (x) (not x))) ; not x
                               (else
                                (lambda args
                                  (error "Operator undefined in Boolean"
                                         operator))))))
                        (simple-operation operator boolean? procedure)))))

;; test cases
(install-arithmetic! boolean-arithmetic)
(+ #t #f) ; Value: #t
(+ #f #f) ; Value: #f
(- #f #t) ; Value: #f
(- #t #f) ; Value: #t 
(* #t #t) ; Value: #t
(* #f #f) ; Value: #f
(negate #t) ; Value: #f
(negate #f) ; Value: #t 



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;         PROBLEM 3.2      ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (a)

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

;; addition
(define (v:+ vec1 vec2)
  (ensure-vector-lengths-match (list vec1 vec2))
  (list->vector
   (map n:+ (vector->list vec1) (vector->list vec2))))
(v:+ #(1 2 3) #(4 5 6)) ; Value: #(5 7 9) 
   
;; negate
(define (v:negate vec) ((vector-element-wise (lambda (x) (n:* x -1))) vec))
(v:negate #(1 -2 -3)) ; Value: #(-1 2 3) 

;; subtraction
(define (v:- vec1 vec2) (v:+ vec1 (v:negate vec2)))
(v:- #(1 2 3) #(4 5 6)) ; Value: #(-3 -3 -3)


;;; (b)

#|
I ended up using n:+ in my addition instead of the arithemtic made available by install-arithmetic! I learned this the hard way as, after completing the previous problem, my definition of + had changed to work on booleans. This made implemeting vector addition difficult.

Using the installed operator may cause changes to the definition depending on what arithmetics are installed whereas the base arithmetic is more likely to be consistent. So, I used n:+ to ensure that the vector operations defined above regardless of which arithmetic is installed.
|#

;;addition
(define (vector-addition-maker +)
  (define (vector-addition vec1 vec2)
    (ensure-vector-lengths-match (list vec1 vec2))
    (list->vector
     (map + (vector->list vec1) (vector->list vec2))))
  vector-addition)
(define v:+ (vector-addition-maker n:+))
(v:+ #(3 4) #(10 10)) ; Value: #(13 14)

;; negate
(define (vector-negation-maker *)  
  (define (vector-negate vec)  
    ((vector-element-wise (lambda (x) (* x -1))) vec))  
  vector-negate)
(define v:negate (vector-negation-maker n:*))
(v:negate #(-10 20 -30 40)) ; Value: #(10 -20 30 -40)

;; subtraction
(define (vector-subtraction-maker + negate)  
  (define (vector-subtract vec1 vec2)  
    (+ vec1 (negate vec2)))  
  vector-subtract)
(define v:- (vector-subtraction-maker v:+ v:negate))
(v:- #(10 20 30) #(30 20 10)) ; Value: #(-20 0 20)


;;; (c) 
;; dot product
(define (vector-dot-product-maker + *)
  (define (vector-dot-product vec1 vec2)
    (ensure-vector-lengths-match (list vec1 vec2))
    (apply + (map * (vector->list vec1) (vector->list vec2))))
  vector-dot-product)
(define v:dot (vector-dot-product-maker n:+ n:*))
(v:dot #(1 2) #(3 4)) ; Value: 11 (1*3 + 2*4 = 3+8 = 11)


;;; (d)
;; magnitude
(define (vector-magnitude-maker + * sqrt)
  (let ((vector-dot-product (vector-dot-product-maker + *)))
    (define (vector-magnitude v) (sqrt (vector-dot-product v v)))
    vector-magnitude))
(define v:magnitude (vector-magnitude-maker n:+ n:* n:sqrt))
(v:magnitude #(3 4)) ; Value: 5 (pythag theorem = 5)


;;; (e)
;; scalar multiplication - doing left and right to help with operation union
; scalar on the left
(define (vector-scalar-multiplication-maker-left *)
  (define (vector-scalar-multiply-left scalar vec)
    (list->vector (map (lambda (x) (* scalar x)) (vector->list vec))))
  vector-scalar-multiply-left)
; scalar on the right
(define (vector-scalar-multiplication-maker-right *) 
  (define (vector-scalar-multiply-right vec scalar) 
    (list->vector (map (lambda (x) (* scalar x)) (vector->list vec)))) 
  vector-scalar-multiply-right)
(define v:mult-left (vector-scalar-multiplication-maker-left n:*))
(v:mult-left 10 #(1 2 3)) ; Value: #(10 20 30)
(define v:mult-right (vector-scalar-multiplication-maker-right n:*))
(v:mult-right #(1 3 5) 2) ; Value: #(2 6 10)


;; extend arithmetic, handle difference cases
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

;; tests
(register-predicate! vec-or-num? 'vector)
(define vector-arithmetic
  (extend-arithmetic vector-extender numeric-arithmetic))
(install-arithmetic! vector-arithmetic)

(+ #(1 2 3) #(4 5 6)) ; Value: #(5 7 9)
(- #(1 2 3) #(4 5 6)) ; Value: #(-3 -3 -3)
(negate #(-1 2 -3 4)) ; Value: #(1 -2 3 -4)
(magnitude #(3 4)) ; Value: 5
(* 2 #(1 3 5)) ; Value: #(2 6 10) 
(* #(1 2 3) #(2 2 2)) ; Value: 12
(* #(1 2 3) 10) ; Value: #(10 20 30)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;         PROBLEM 3.3      ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; helpers
(define (unit-circle x) (vector (sin x) (cos x)))

(define vector-trig (vector sin cos))

(define vec-before-func
  (extend-arithmetic function-extender
    (extend-arithmetic vector-extender combined-arithmetic)))

(define func-before-vec
  (extend-arithmetic vector-extender
    (extend-arithmetic function-extender combined-arithmetic)))

;; test vec-before-func
(install-arithmetic! vec-before-func)
(display ((magnitude unit-circle) 'a)) ; The object (sin a), passed as the first argument to integer-zero?, is not the correct type
(display ((magnitude vector-trig) 'a)) ; The object #[compound-procedure 1], passed as the first argument to integer-zero?, is not the correct type

;; test func-before-vec
(install-arithmetic! func-before-vec)
(display ((magnitude unit-circle) 'a)) ; Inapplicable operation: magnitude (#((sin a) (cos a)))
(display ((magnitude vector-trig) 'a)) ; The object #[compound-procedure 1], passed as the first argument to integer-zero?, is not the correct type

#|
Looking at the errors (commented next to the commands) we can get insight into what happened. In vec-before-func, we see that ((magnitude unit-circle) 'a) performed some evaluation before throwing an error. It looks like it is returning a vector of #((sin a) (cos a)), but now the magnitude cannot handle non-numerical values (probably my fault on implementation).

In func-before-vec, it looks like the functione extension is messing up function-containing vectors. As in, magnitude will not work with the extension.

I think vec-before-func could work if there were small changes to how symbols are handled. func-before-vec seems more difficult since it looks as if it is nesting a vector in a function in a vector (#((sin a) (cos a))). I don't think that means it is impossible, but would probably need a lot of specific cases to handle this issue. I think vec-before-func would be easier to generalize and be a better overall solution.
|#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;         PROBLEM 3.a      ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
I use a lot of Python at MIT which has the helpful dunder methods when creating new objects. In Python, I could create a new class Vector and overwrite the +, -, and * easily with dunder methods. As compared to writing the vector arithmetic in scheme though, this has more handling of edge cases:

------------------------ Python Implementation ------------------------
import math

class Vector:
    def __init__(self, values):
        self.values = values

    def __repr__(self):
        return f"Vector({self.values})"

    def __str__(self):
        return str(self.values)

    def __add__(self, other):
        if isinstance(other, Vector):
            return Vector([x + y for x, y in zip(self.values, other.values)])
        raise TypeError("Unsupported operation")

    def __sub__(self, other):
        if isinstance(other, Vector):
            return Vector([x - y for x, y in zip(self.values, other.values)])
        raise TypeError("Unsupported operation")

    def __mul__(self, other):
        if isinstance(other, Vector):
            return sum(x * y for x, y in zip(self.values, other.values))  # dot product
        elif isinstance(other, (int, float)):  # scalar multiplication
            return Vector([x * other for x in self.values])
        raise TypeError("Unsupported operation")

    def __rmul__(self, other):  # new function to allow scalar mult
        return self * other

    def __neg__(self):  
        return Vector([-x for x in self.values])

    def magnitude(self):
        return math.sqrt(sum(x ** 2 for x in self.values))

## test
vec1 = Vector([1, 2, 3])
vec2 = Vector([3, 4, 5])
# addition
print(vec1 + vec2)  # [4, 6, 8]
# subtraction
print(vec2 - vec1)  # [2, 2, 2]
# multiplication
print(vec1 * vec2)  # 26 
print(vec1 * 2)  # [2, 4, 6]
print(2 * vec1)  # [2, 4, 6] (works with __rmul__)
# negate
print(-vec1)  # [-1, -2, -3]
# magnitude
print(Vector([3,4]).magnitude())  # 5.0

------------------------ Python Implementation ------------------------

Now having worked with scheme, Python feels clunky because different operations are called in different ways. For example, you can call addition, subtraction, mutliplication, and negation noramlly, but magnitude is called with a different syntax as there is no dunder for it. Scheme is really nice as all functions are called the same way. Additionally, I had to import a math package for python to handle just magintude. In a more complicated arithmetic system, additional imports are annoying.
|#
