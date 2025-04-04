#|
Elise Harvey
Kerb: erharvey
6.5151 ps06
|#


(load "~/Desktop/6.5151/sdf/manager/load.scm")
(manage 'new 'generic-interpreter)
(load "~/Desktop/6.5151/sdf/non-strict-arguments/general-procedures")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;         PROBLEM 5.3      ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cos 0.6) ; .8253356149096783
((vector cos sin) 0.6) ; Unknown procedure type ...

#|
As with previous problem sets, this code has been added to the code loaded above, but a copy has been added below.
|#

(define (vector-of-procedures? obj)
  (and (vector? obj)
       (every procedure? (vector->list obj))))

(define-generic-procedure-handler g:apply
  (match-args vector-of-procedures? operands? environment?)
  (lambda (procedure operands calling-environment)
    (let ((evaluated-args (eval-operands operands calling-environment)))
      (list->vector
       (map (lambda (proc)
              (g:apply proc
                       (map (lambda (arg) `(quote ,arg))
                            evaluated-args)
                       calling-environment))
            (vector->list procedure))))))

;; testing! (in eval)
((vector cos sin) 0.6) ; #(.8253356149096783 .5646424733950354) yay!



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;         PROBLEM 5.a      ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
The goal in this problem is to create a define-macro that allows us to define maros at the source level. These should be expanded before being evalauted. To do this, we will build a constructor that will tag the macros defined. I am choosing to tag them since we need a way to distinguish what is a user-define macro before evaluation (since this is non-hygenic, the user could re-write some other existing value). Then, we will make sure the macros are expanded before evaluation.
|#

;; NEW handling macros
(define (define-macro name transformer)
  (define-variable! name
    (make-macro transformer)
    (current-environment)))

(define (expand-macro macro expression)
  ((macro-transformer macro) expression))

(define (make-macro transformer)
  (list 'macro transformer))

(define (macro? obj)
  (and (pair? obj) (eq? (car obj) 'macro)))

(define (macro-transformer macro)
  (cadr macro))

;; coderef: macros
(define-generic-procedure-handler g:eval
  (match-args application? environment?)
  (lambda (expression environment)
    (let ((operator (g:eval (operator expression) environment)))
      (if (macro? operator)
          ;; expand and re-evaluate
          (g:eval (expand-macro operator expression) environment)
          ;; apply as usual
          (g:apply operator (operands expression) environment)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;         PROBLEM 5.9      ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
a. I think it could be beneficial to not memoize in a context where the value needs to be recomputed at different times/states. Maybe some of the values used when evaluating change with time/state, so we would have to recompute.

b. We cannot use cons when creating the definition for kons as cons is strict and cannot support the memoization/laziness that we need with cons.

c. I would imagine that using kons instead of cons would make debugging a nightmare. Computer bugs could be hard to trace or hidden by the lazy evaluation. Also personally, cons is easier to understand and use. I think kons fits some very specific use cases and we could just define and use it when needed.
|#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;         PROBLEM 5.10     ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
a. For this, I think it would be easiest for the syntax to follow (param-name restrict-to <predicate> lazy memo). To be honest, I have no idea what BNF is and am struggling to figure out what this means for what I need to write in this problem (sorry).
|#


#|
b. The goal is to recognize when restrict-to is used and apply the given predicate when called. This solution got the definition to work, but calling new functions with restricted parameters caught issues with which g:apply was being called.
|#

;; coderef: restricted parameters
(define-generic-procedure-handler g:apply
  (match-args compound-procedure? operands? environment?)
  (lambda (procedure operands calling-environment)
    (let ((params (procedure-parameters procedure)))
      (if (not (= (length params) (length operands)))
          (error "Incorrect number of arguments"))
      (let ((evaluated-args (eval-operands operands calling-environment)))
        (let ((names '())
              (values '()))
          (for-each (lambda (param arg)
                      (let ((name (cdr (assoc 'name param)))
                            (decls (cdr (assoc 'declarations param))))
                        ;; declarations
                        (for-each
                         (lambda (decl)
                           (if (and (pair? decl)
                                    (eq? (car decl) 'restrict-to))
                               (let ((pred (cadr decl)))
                                 (if (not ((eval pred) arg))
                                     (error "Argument does not satisfy restriction"
                                            name pred arg)))
                               #f)) ; do nothing 
                         decls)
                        ;; bindings
                        (set! names (cons name names))
                        (set! values (cons arg values))))
                    params evaluated-args)
          (g:eval (procedure-body procedure)
                  (extend-environment (reverse names)
                                      (reverse values)
                                      (procedure-environment procedure))))))))

(define (parse-parameter param)
  (cond ((symbol? param)
         `((name . ,param) (declarations . ())))
        ((pair? param)
         (let ((name (car param))
               (decls (cdr param)))
           `((name . ,name) (declarations . ,decls))))
        (else
         (error "Invalid parameter format" param))))

(define (make-compound-procedure parsed-params body env)
  (list 'compound-procedure parsed-params body env))

(define (compound-procedure? p)
  (and (pair? p) (eq? (car p) 'compound-procedure)))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

