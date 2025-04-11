#|
Elise Harvey
kerb: erharvey
6.5151 ps07
|#

(load "~/Desktop/6.5151/sdf/manager/load.scm")
(manage 'new 'compiling-to-execution-procedures)
(manage 'new 'exploratory-behavior)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;         PROBLEM 5.14     ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
For this problem, we are assuming we have access to different declarations/procedures. First, we will assume that there is some declaration that would allow us to tell the analyzer that symbols have a given meaning. This would allow us to tell the analyzer that these symbols can be evalauted before runtime. Let's call this declaration has-known-binding such that we can say (has-known-binding symb) and the anaylzer would know that symb has a value. Next, we wull assume the analyzer has a procedure that will find the finding of known symbols and can retrieve the value. For this problem, I will assume this is called get-known-binding and would work as (get-known-binding symb). With these, we can implement constant folding for our system. I think this can be done by adding handling for constant folding in the analyze-application procdure in analyze.scm.
|#

(define (analyze-application expression)
  ;; constant folding if possible
  (if (and (pair? expression)
           (symbol? (car expression))
           (has-known-binding (car expression)))
      (let* ((proc (get-known-binding (car expression)))
             (vals (map (lambda (arg)
                          (if (self-evaluating? arg)
                              arg
                              (text-of-quotation arg)))
                        (cdr expression)))
             (result (apply proc vals)))
        (lambda (env) result)) ; result should be constant folded!?
      ;; original analyze-application if constant folding does not apply
      (let ((operator-exec (analyze (operator expression)))
            (operand-execs (map analyze (operands expression))))
        (lambda (environment)
          (x:apply (x:advance (operator-exec environment))
                   operand-execs
                   environment)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;         PROBLEM 5.15     ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
In this problem, I wanted to see if simple implementations could noticably save extra time. I will implement the identity math functions (throwback to our arithmetics). I will make sure that I can see the runtime so we can do a comparison of before and after the optimization.
|#

;; first testing no optimization, got 9.999999999999787e-3 ms
(let ((start (runtime)))
  (let ((result (x:eval '(+ (* 1 12345) (+ 12345 0)) the-global-environment)))
    (display result) (newline)
    (display "Time: ") (display (- (runtime) start)) (display " ms")))
;; adding indentity math to analyze-application
(define (analyze-application expression)
  ;; identity optimization!
  (let ((op (car expression))
        (args (cdr expression)))
    (cond
     ;; (+ x 0) = x
     ((and (eq? op '+)
           (= (length args) 2)
           (equal? (cadr expression) 0))
      (analyze (car args)))
     ;; (* 1 x) = x
     ((and (eq? op '*)
           (= (length args) 2)
           (equal? (car args) 1))
      (analyze (cadr args)))
     ;; original analyze-application for other cases
     (else
      (let ((operator-exec (analyze (operator expression)))
            (operand-execs (map analyze (operands expression))))
        (lambda (environment)
          (x:apply (x:advance (operator-exec environment))
                   operand-execs
                   environment)))))))

;; testing with optimization, got 1.0000000000001563e-2 ms

#|
I ran the code that printed out the time after the opertions repeatedly and the times were consistent for the before and the after. I think it is cool that a small addition of the identity applciation speads this up by an order of 10, but this was a really small optimization. If I have more time at the end of this pset, I think I would like to attempt a larger optimization and check the runtime differences. I also might consider a way to generalize my solution. It is basically hardcoding the optimization and it would be cooler to have a more general way to do this. I don't know what that would look like though...
|#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;         PROBLEM 5.17     ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
For this problem, we are going to use amb to search possible assignments for seating arrangements. I am going to create seats 0 through 5, but fix one of the people at seat 0 such that I am not generating duplicate arrangements but rotated. If we fix Alyssa (as 0), then we can also fix Ben at 3 (since he is opposite of Alyssa). From here, we will implement the rest of the constraints and let it run. This helps us ignore solutions by rotation and as a bonus will make the program run faster as we are looking/trying less assignments.
|#

(define (one-of lst)
  (if (null? lst)
      (amb)
      (amb (car lst)
           (one-of (cdr lst)))))

(define (distinct l)
  (cond ((null? l) true)
	((null? (cdr l)) true)
	((member (car l) (cdr l)) false)
	(else (distinct (cdr l)))))

(define (require p)
  (if (not p) (amb)))

(define (map f l)
  (if (null? l)
      '()
      (cons (f (car l))
	    (map f (cdr l)))))

(define (to-right n)
  (modulo (+ n 1) 6))

(define (better-hand? position1 position2 values)
  (> (cdr (assv position1 values))
     (cdr (assv position2 values))))

(define (except set exceptions)
  (one-of (lset-difference eqv? set exceptions)))

(define (assignments places values)
  (if (null? places)
      '()
      (let ((choice (one-of values)))
	(cons (cons (car places) choice)
	      (assignments (cdr places)
			   (delv choice values))))))

(define (card-table)
  (let (
	(eva 0) ; fix eva at 0
	(ben 3) ; constraint 1 -> ben is opposite of eva
	(alyssa (amb 1 2 4 5)))
    (let ((louis (except '(1 2 4 5) (list alyssa))))
      (let ((cy (except '(1 2 4 5) (list alyssa louis))))
	(let ((lem (except '(1 2 4 5) (list alyssa louis cy))) 
	      (values (assignments '(0 1 2 3 4 5)
				   '(10 20 30 40 50 60))))
	  (let ((men (list ben louis cy lem)) ; men
		(women (list eva alyssa))) ; women
	    (require (distinct (list alyssa louis cy lem))) ; they gotta be distinct

            ;; constraint 2 -> the man at Alyssa's right has a stronger hand than Lem has.
	    (require (memv (to-right alyssa) men))
	    (require (not (= (to-right alyssa) lem)))
	    (require (better-hand? (to-right alyssa) lem values))

            ;; constraint 3 -> the man at Eva's right has a stronger hand than Ben has
	    (require (memv (to-right eva) men))
	    (require (better-hand? (to-right eva) ben values))

            ;; constraint 4 -> the man at Ben's right has a stronger hand than Cy has
            ;; constraint 5 -> the man at Ben's right has a stronger hand than Eva has.
	    (require (memv (to-right ben) men))
	    (require (not (= (to-right ben) cy)))
	    (require (better-hand? (to-right ben) cy values))
            (require (better-hand? (to-right ben) eva values))

            ;; constraint 6 -> the woman at Lem's right has a stronger hand than Cy has.
	    (require (memv (to-right jake) women))
	    (require (better-hand? (to-right jake) cy values))

            ;; constraint 7 -> the woman at Cy's right has a stronger hand than Louis has.
	    (require (memv (to-right cy) women))
	    (require (better-hand? (to-right cy) louis values))

	    (list eva ben alyssa louis cy lem)))))))

#|
I feel like I am losing it. I ended up following an example from SDF but now my computer is throwing an error when it tries to use amb. I have loaded the files referenced on the pset  and I cannot figure out what else I am missing. It says "Unbound variable: amb" which doesn't make sense to me. If you are reading this, I was not able to debug in time for submission. I had a workaround that I thought worked for 5.19, but am now wondering if this was not working the whole time... I saved this for last and that was a mistake :(
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;         PROBLEM 5.18     ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
To do this, we just need to wrap the existing continuation (when it fails).
|#

(define (if-fail? exp)
  (and (pair? exp) (eq? (car exp) 'if-fail)))

(define (if-fail-try exp) (cadr exp))
(define (if-fail-fallback exp) (caddr exp))

(define (analyze-if-fail exp)
  (let ((try-proc (a:analyze (if-fail-try exp)))
        (fallback-proc (a:analyze (if-fail-fallback exp))))
    (lambda (env succeed fail)
      (try-proc env succeed
                (lambda ()
                  (fallback-proc env succeed fail))))))
;; added a check to the default analyze
(define (default-analyze exp)
  (cond ((if-fail? exp) (analyze-if-fail exp)) ; NEW
	((application? exp) (analyze-application exp))
        (else (error "Unknown expression type" exp))))

;; testing!
(define succeed (lambda (value fail) (begin (display value) (newline) value)))
(define fail (lambda () (display "FAIL\n") 'fail))

(a:eval
 '(if-fail
    (let ((x (amb)))
      x)
    'you-should-see-me)
 the-global-environment
 succeed
 fail) ; Value: you-should-see-me



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;         PROBLEM 5.19     ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set
(let ((pairs '()))
  (if-fail
   (let ((p (prime-sum-pair '(1 3 5 8) '(20 35 110))))
     (set! pairs (cons p pairs))
     (amb))
   pairs))
#|
This gave me an error as it had to abort because of memory issues.
|#

;; maybe set
(let ((pairs '()))
  (if-fail
   (let ((p (prime-sum-pair '(1 3 5 8) '(20 35 110))))
     (maybe-set! pairs (cons p pairs))
     (amb))
   pairs))

#|
So this also gave me an error (had to abort for memory). I am guessing that this was supposed to show how maybe-set! is better for back-tracking with amb.
|#
