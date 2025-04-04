#| -*-Scheme-*-

Copyright (C) 2019, 2020, 2021 Chris Hanson and Gerald Jay Sussman

This file is part of SDF.  SDF is software supporting the book
"Software Design for Flexibility", by Chris Hanson and Gerald Jay
Sussman.

SDF is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

SDF is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with SDF.  If not, see <https://www.gnu.org/licenses/>.

|#

;;;; Experimental interpreter

;;;  Separating analysis from execution.
;;;    Continuation-passing style execution.
;;;  Generic analysis, with lazy and lazy-memo parameters.
;;;    Simple compound procedures (Scheme) separated.
;;;    Complex compound procedure parameters
;;;      are parsed at runtime, ugh.
;;;      no rest parameters.

(define (l:eval expression environment continue)
  ((analyze expression) environment continue))

(define (analyze expression)
  (make-executor (l:analyze expression)))

(define (default-analyze expression)
  (if (not (application? expression))
      (error "Unknown expression type" expression))
  (analyze-application expression))

(define l:analyze
  (simple-generic-procedure 'l:analyze 1 default-analyze))

(define (analyze-application expression)
  (let ((operator-exec (analyze (operator expression)))
        (operand-execs (map analyze (operands expression))))
    (lambda (environment continue)
      (l:execute-strict operator-exec environment
        (extend-continuation
          (lambda (proc)
            (l:apply proc
                     operand-execs
                     environment
                     continue)))))))

(define (l:execute-strict executor env continue)
  (executor env
    (extend-continuation
      (lambda (value)
        (l:advance value continue)))))

(define (analyze-self-evaluating expression)
  (lambda (environment continue)
    (declare (ignore environment))
    (continue expression)))

(define-generic-procedure-handler l:analyze
  (match-args self-evaluating?)
  analyze-self-evaluating)


(define (analyze-variable expression)
  (lambda (environment continue)
    (continue
     (lookup-variable-value expression environment))))

(define-generic-procedure-handler l:analyze
  (match-args variable?)
  analyze-variable)


(define (analyze-quoted expression)
  (let ((qval (text-of-quotation expression)))
    (lambda (environment continue)
      (declare (ignore environment))
      (continue qval))))

(define-generic-procedure-handler l:analyze
  (match-args quoted?)
  analyze-quoted)

(define (simple-parameter-list? vars)
  (or (null? vars)
      (symbol? vars)
      (and (pair? vars)
           (symbol? (car vars))
           (simple-parameter-list? (cdr vars)))))

(define (analyze-lambda expression)
  (let ((vars (lambda-parameters expression))
        (body-exec (analyze (lambda-body expression))))
    (if (simple-parameter-list? vars)
        (lambda (environment continue)
          (continue
           (make-simple-compound-procedure vars
                                           body-exec
                                           environment)))
        (lambda (environment continue)
          (continue
           (make-complex-compound-procedure vars
                                            body-exec
                                            environment))))))

(define-generic-procedure-handler l:analyze
  (match-args lambda?)
  analyze-lambda)

;;; analyze-if is extended with a hook to allow the
;;; interpolation of work between the evaluation of the
;;; predicate part of the expression and the choice of
;;; consequent or alternative to be evauated.

;;; This hack was inspired by the requirement to allow the
;;; layers of the predicate value to contribute to the
;;; layers of the value of the consequent or the
;;; alternative.  For example, provenance of the predicate
;;; value must be combined with the provenance of the value
;;; of the consequent or the alternative to produce the
;;; provenance of the answer.

;;; To preserve proper tail-recursion the collection of
;;; values from the layers of the predicate and the chosen
;;; consequent or alternative must be done without creating
;;; a new continuation.  So we extend continuations to allow 
;;; them to use continuation marks to accumulate the values
;;; from the contributing layers without interpolating new
;;; continuations.  

;;; The l:if-hook is a generic procedure that allows special
;;; handlers for things like layering.  See layered-data.scm
;;; for the details.

;;; The details are very subtle.

(define (analyze-if expression)
  (let ((predicate-exec (analyze (if-predicate expression)))
        (consequent-exec (analyze (if-consequent expression)))
        (alternative-exec (analyze (if-alternative expression))))
    (lambda (environment continue)
      (l:execute-strict predicate-exec environment
        (extend-continuation
          (lambda (p-value)
            (l:if-hook p-value continue
              (lambda (p-value* continue*)
                ((if p-value* consequent-exec alternative-exec)
                 environment continue*)))))))))

(define l:if-hook
  (simple-generic-procedure 'l:if-hook 3
    (lambda (p-value continue receiver)
      (receiver p-value continue))))

(define-generic-procedure-handler l:analyze
  (match-args if?)
  analyze-if)

(define (analyze-begin expression)
  (reduce-right
   (lambda (exec1 exec2)
     (lambda (environment continue)
       (exec1 environment
              (extend-continuation
                (lambda args
                  (declare (ignore args))
                  (exec2 environment continue))))))
   #f
   (map analyze
        (let ((exps (begin-actions expression)))
          (if (null? exps)
              (error "Empty sequence"))
          exps))))

(define-generic-procedure-handler l:analyze
  (match-args begin?)
  analyze-begin)

(define (analyze-assignment expression)
  (let ((var
         (assignment-variable expression))
        (value-exec
         (analyze (assignment-value expression))))
    (lambda (environment continue)
      (value-exec environment
        (extend-continuation
          (lambda (val)
            (set-variable-value! var val environment)
            (continue `(,var assigned))))))))

(define-generic-procedure-handler l:analyze
  (match-args assignment?)
  analyze-assignment)


(define (analyze-definition expression)
  (let ((var (definition-variable expression))
        (value-exec (analyze (definition-value expression))))
    (lambda (environment continue)
      (value-exec environment
        (extend-continuation
          (lambda (val)
            (define-variable! var val environment)
            (continue `(,var defined))))))))

(define-generic-procedure-handler l:analyze
  (match-args definition?)
  analyze-definition)

;;; Macros (definitions are in syntax.scm)

(define-generic-procedure-handler l:analyze
  (match-args cond?)
  (compose analyze cond->if))

(define-generic-procedure-handler l:analyze
  (match-args let?)
  (compose analyze let->combination))

(define-generic-procedure-handler l:analyze
  (match-args let*?)
  (compose analyze let*->let-nest))

;;; Operand evaluation

(define (l:execute-operands execute operands env continue)
  (map-continuation (lambda (operand continue)
                      (execute operand env continue))
                    operands
                    continue))

(define (map-continuation f things continue)
  (if (null? things)
      (continue '())
      (f (car things)
         (extend-continuation
           (lambda (val)
             (map-continuation f (cdr things)
               (extend-continuation
                 (lambda (vals)
                   (continue (cons val vals))))))))))

;;; Application

(define (default-apply procedure operand-execs
                       environment continue)
  (declare (ignore operand-execs environment))
  (error "Unknown procedure type" procedure)
  barf)

(define l:apply
  (simple-generic-procedure 'l:apply 4 default-apply))

(define (primitive-or-simple-procedure? p)
  (or (strict-primitive-procedure? p)
      (simple-compound-procedure? p)
      (call/cc? p)
      (applyer? p)))
(register-predicate! primitive-or-simple-procedure?
                     'primitive-or-simple-procedure)

(define (interpreter-procedure? p)
  (or (primitive-or-simple-procedure? p)
      (complex-compound-procedure? p)))

(register-predicate! interpreter-procedure?
                     'interpreter-procedure)

(define-generic-procedure-handler l:apply
  (match-args primitive-or-simple-procedure?
              executors?
              environment?
              extended-continuation?)
  (lambda (procedure operand-execs env continue)
    (l:execute-operands l:execute-strict operand-execs env
      (extend-continuation
        (lambda (args)
          (l:apply-strict procedure
                          args
                          continue))))))

;;; Traditional APPLY, as in Scheme
(define (l:apply-strict procedure args continue)
  (cond ((strict-primitive-procedure? procedure)
         (continue (apply-primitive-procedure procedure args)))
        ((simple-compound-procedure? procedure)
         (l:compound-apply procedure args continue))
        ((call/cc? procedure)
         (l:deliver-continuation (car args) continue))
        ((applyer? procedure)
         ((applyer-procedure procedure)
          procedure args continue))
        (else
         (error "Bad strict procedure" procedure args)
         'to-retain-stack)))

(define (l:compound-apply procedure args continue)
  (let ((params (simple-procedure-parameters procedure))
        (penv (simple-procedure-environment procedure))
        (body-exec (simple-procedure-body procedure)))
    (body-exec (extend-environment params args penv)
               continue)))

(define-generic-procedure-handler l:apply
  (match-args complex-compound-procedure? executors?
              environment? extended-continuation?)
  (lambda (procedure operand-execs calling-environment continue)
    (if (not (n:= (length
                   (complex-procedure-parameters procedure))
                  (length operand-execs)))
        (error "Wrong number of operands supplied"))

    (let ((params (complex-procedure-parameters procedure))
          (penv (complex-procedure-environment procedure))
          (body-exec (complex-procedure-body procedure)))
      (let ((names (map procedure-parameter-name params)))
        (l:execute-operands
         (lambda (operand env continue)
           (l:handle-operand (car operand)
                             (cdr operand)
                             env
                             continue))
         (map cons params operand-execs)
         calling-environment
         (extend-continuation
           (lambda (args)
             (body-exec (extend-environment names args penv)
                        continue))))))))

(define l:handle-operand
  (simple-generic-procedure 'l:handle-operand 4
    (lambda (parameter operand-exec environment continue)
      (declare (ignore parameter))
      (operand-exec environment continue))))

(define-generic-procedure-handler l:handle-operand
  (match-args lazy? executor? environment?
              extended-continuation?)
  (lambda (parameter operand-exec environment continue)
    (declare (ignore parameter))
    (continue (postpone operand-exec environment))))

(define-generic-procedure-handler l:handle-operand
  (match-args lazy-memo? executor? environment?
              extended-continuation?)
  (lambda (parameter operand-exec environment continue)
    (declare (ignore parameter))
    (continue (postpone-memo operand-exec environment))))

(define-generic-procedure-handler procedure-parameter-name
  (match-args pair?)
  car)

(define l:advance
  (simple-generic-procedure 'l:advance 2
    (lambda (x continue) (continue x))))

(define-generic-procedure-handler l:advance
  (match-args postponed? extended-continuation?)
  (lambda (object continue)
    (let ((pexpr (postponed-expression object))
          (penv (postponed-environment object)))
      (l:execute-strict pexpr penv continue))))

(define-generic-procedure-handler l:advance
  (match-args postponed-memo? extended-continuation?)
  (lambda (object continue)
    (let ((pexpr (postponed-expression object))
          (penv (postponed-environment object)))
      (l:execute-strict pexpr penv
        (extend-continuation
          (lambda (val)
            (advance-memo! object val)
            (continue val)))))))

(define-generic-procedure-handler l:advance
  (match-args advanced-memo? extended-continuation?)
  (lambda (object continue)
    (continue (advanced-value object))))
