#|
Elise Harvey
Kerb: erharvey
6.5151 ps05
|#

(load "~/Desktop/6.5151/sdf/manager/load.scm")
(manage 'new 'term)
(manage 'add 'design)
(manage 'add 'unification)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;         PROBLEM 4.4      ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(algebra-2 '(+ (* 4 x) (* 3 x))) ; Value: (+ (* 3 x) (* 4 x))
;; We want this to be (* 7 x)

(define algebra-3
  (rule-simplifier
   (list
    ;; Sums (algebra-2)
    (rule `(+ (? a)) a)
    (rule `(+ (?? a) (+ (?? b)) (?? c))
          `(+ ,@a ,@b ,@c))
    (rule `(+ (?? a) (? y) (? x) (?? b))
          (and (expr<? x y)
               `(+ ,@a ,x ,y ,@b)))
    ;; Products (algebra-2) 
    (rule `(* (? a)) a)
    (rule `(* (?? a) (* (?? b)) (?? c))
          `(* ,@a ,@b ,@c))
    (rule `(* (?? a) (? y) (? x) (?? b))
          (and (expr<? x y)
               `(* ,@a ,x ,y ,@b)))
    ;; Distributive law (algebra-2) 
    (rule `(* (?? a) (+ (?? b)) (?? c))
          `(+ ,@(map (lambda (x) `(* ,@a ,x ,@c)) b)))
    ;; Numerical simplifications below (algebra-2) 
    (rule `(+ 0 (?? x)) `(+ ,@x))
    (rule `(+ (? x ,number?) (? y ,number?) (?? z))
          `(+ ,(+ x y) ,@z))
    (rule `(* 0 (?? x)) 0)
    (rule `(* 1 (?? x)) `(* ,@x))
    (rule `(* (? x ,number?) (? y ,number?) (?? z))
          `(* ,(* x y) ,@z))
    ;; NEW Collecting Terms
    (rule `(+ (?? a) (* (? y ,number?) (?? vars)) (* (? x ,number?) (?? vars)) (?? b))
          `(+ ,@a (* ,(+ x y) ,@vars) ,@b))
    (rule `(+ (?? a) (* (?? vars)) (* (? x ,number?) (?? vars)) (?? b))
          `(+ ,@a (* ,(+ x 1) ,@vars) ,@b))
    (rule `(+ (?? a) (* 0 (?? c)) (?? b))
          `(+ ,@a ,@b))
    )))

;; testing it!
(algebra-3 '(+ (* 4 x) (* 3 x))) ; Value: (* 7 x)
(algebra-3 '(+ y (* x -2 w) (* x 4 y) (* w x) z (* 5 z) (* x w) (* x y 3))) ; Value: (+ y z (* 5 z) (* 7 x y))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;         PROBLEM 4.5      ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
In this example, we are telling the procedure to return #f when it succeeds which causes it to backtrack. The #f stops the procedure from halting and effectively backtracks to find another solution. Since we do this for each time it succeeds, we will exhaust all matches in this case.
|#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;         PROBLEM 4.a      ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
This problem needs us to match values into a single element if possible, and if not, match it as a list in the order of the values. Note that these have been added to matcher.scm and a copy has been added here as well.
|#

(define (match:compile-pattern pattern)
  (cond ((match:var? pattern)
         (case (match:var-type pattern)
           ((?) (match:element pattern))
           ((??) (match:segment pattern))
           ((\$) (match:collect pattern)) ; NEW HANDLING FOR COLLECT
           (else (error "Unknown var type:" pattern))))
        ((list? pattern)
         (match:list (map match:compile-pattern pattern)))
        (else
         (match:eqv pattern))))

(define (match:collect variable)
  (define (collect-match data dictionary succeed)
    (and (pair? data)
         (match:satisfies-restriction? variable (car data))
         (let* ((current-value (car data))
                (binding (match:lookup variable dictionary)))
           (if binding
               (let ((existing-value (match:binding-value binding)))
                 (cond
                  ;; if value already matched exactly
                  ((or (equal? existing-value current-value)
                       (and (list? existing-value)
                            (member current-value existing-value)))
                   (succeed dictionary 1))
                  ;; if already a list
                  ((list? existing-value)
                   (succeed
                    (match:extend-dict variable
                                       (cons current-value existing-value)
                                       dictionary)
                    1))
                  ;; not a list yet
                  (else
                   (succeed
                    (match:extend-dict variable
                                       (list current-value existing-value)
                                       dictionary)
                    1))))
               ;; first one
               (succeed
                (match:extend-dict variable
                                   current-value
                                   dictionary)
                1)))))
  collect-match)

(define match:var-types '(? ?? $))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;         Problem 4.8a     ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
Lists enabled us to check and access sequences using car and cdr. To enable other data types, we would have to replace these with a generic handler that can access based on the type. We could tag data types to help with distinguish. Basically, we need to generalize our structure and replace a lot of the procedures such that simplifications, access, modifications, etc. depend on the type of data.
|#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;         PROBLEM 4.14     ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#|
When we pass the identity function in our unifier, it intiially unifies and basically tells the system that it should expect a number with this procedure. This is because id is automatically type inferenced to be expecting a number. We want the system to treat ied as a fresh version each time it is passed in, so there are no previous assumptions working agaisnt future calls.

Rather than unifying with a type, we could assign some general symbolic instead. We would effectively be generalizing the procedure types and aim to store more of a template instead. These templates can be updated at each use rather than enforcing future calls.
|#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;         PROBLEM 4.15     ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
For the example of map, we want the system to be able to take other types as parameters. This would mean map would say that it takes a function and a list to return a different list (something like the below).

(type:procedure
  ((type:procedure ((?L1)) ?L2) (type:list ?L1))
(type:list ?L2))

Thus, we need to be able to represent some compound typing that enables one type to be paramtrized by another. To do this, we would need to add the capability to construct such types and update the existing unifier such that it can unify these types. I think this would be best done recursively by recursing into the parameters/types inside. It would match exterior types and recurse in. 
|#
