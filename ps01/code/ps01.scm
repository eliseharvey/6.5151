#|
Elise Harvey
Kerb: erharvey
6.5151 ps01
|#


(load "Desktop/6.5151/ps01/code/load.scm")
(load "Desktop/6.5151/ps01/code/regexp.scm") 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;         PROBLEM 1.1      ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Q: Define Scheme procedures r:* and r:+ to take a pattern and iterate it as necessary.  This can be done in terms of r:repeat.
(define (r:* expr)
  (r:repeat 0 #f expr)) ; need 0 or more repeats

(define (r:+ expr)
  (r:repeat 1 #f expr)) ; need 1 or more repeats

;;; test cases
;; r*: * after the character means zero or more occurences
(display (r:* "abc")) ; ]=> \(abc*\)
;; Confirming output from terminal matches
; grep '\(abc*\)' Desktop/6.5151/ps01/code/tests.txt --> [00]. abc [04]. abdabec
(if (equal? (r:grep (r:* "abc") 
                    "Desktop/6.5151/ps01/code/tests.txt")
            '("[00]. abc" "[04]. abdabec"))
    (display "PASS\n") 
    (display "FAIL\n")) ; PASS

;; r+: * after second means it matches first and then zero or more (two versions of above, one match and one with the *)
(display (r:+ "cat")) ; ]=> \(catcat*\)
;; Confirming output from terminal matches
; grep '\(catcat*\)' Desktop/6.5151/ps01/code/tests.txt --> [10]. catcatdogdog [12]. catcatcatdogdogdog
(if (equal? (r:grep (r:+ "cat") "Desktop/6.5151/ps01/code/tests.txt")
            '("[10]. catcatdogdog" "[12]. catcatcatdogdogdog"))
    (display "PASS\n") 
    (display "FAIL\n")) ; PASS



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;         PROBLEM 1.2      ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Q: Let’s very briefly consider each proposal

#|
a. We see that Louis's recommendation will lead us into an infinite loop that will forever call (r:repeat 0 1 expr).

b. While Alyssa's idea would work, it could be computationally expensive to create. With the example, her idea would generate every possible expression between min and max. This easily becomes very large and would be inefficient. Bonnie's idea of using '?' would easily fit into the pre-defined function of r:repeat, be shorter expression than Alyssa's, and would still work as we need it to.

c. Ben's solution would utilize the curly braces that would allow us to specify exact number of repeats, a minimum, or a minimum and a maximum. Furthermore, this syntax is part of the Basic Regular Expressions (BRE) instead of just the Extended Regular Expressions (ERE), making this a better generalized solution. Lastly, as compared to Bonnie's solution, Ben's solution will be more a more compact expression, making it easier to read.

d. See below
|#
(define (r:repeat min max expr)
  (r:seq expr
         (string-append
           "\\{"
           (number->string min)
           (if (number? max) ; if given a max (not #f). Note: assuming max>0 and max>min
               (if (= max min) ; if max=min, we want an exact number (no comma)
                   ""
                   (string-append "," (number->string max)))
               ",")
           "\\}")))
;; test cases
(display (r:repeat 1 2 "a")) ; \(a\{1,2\}\) (CASE: range of values)
(display (r:repeat 2 #f "b")) ; \(b\{2,\}\) (CASE: no max)
(display (r:repeat 3 3 "c")) ; \(c\{3\}\) (CASE: exact number)
;; Confirming output from terminal matches
; grep -E '(cat){3,5}' Desktop/6.5151/ps01/code/tests.txt --> [12]. catcatcatdogdogdog
(if (equal? (r:grep (r:repeat 3 5 (r:quote "cat")) "Desktop/6.5151/ps01/code/tests.txt")
            '("[12]. catcatcatdogdogdog"))
    (display "PASS\n") 
    (display "FAIL\n")) ; PASS



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;         PROBLEM 1.3      ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(display (r:seq (r:quote "a") (r:dot) (r:quote "c"))) ; \(\(a\).\(c\)\) -> ((a) . (c)) should be (a.c)?

;;; Q: Edit our program to eliminate as much of the unnecessary nesting as you can.  Caution: there are subtle cases here that you have to watch out for.  What is such a case?

#|
There are a few things we need to consider in this problem. We need to find  a way to remove the extra parentheses that procedures like r:quote introduce. For this, single characters and already grouped expressions do not need further nesting, so we can check when adding wrapping the expression. On the other hand, we want the author to be able to explicitly paranthesize some expressions, so we need to maintain some mechanism that allows the author to deliberately wrap expressions. For this, we will leave r:seq as this mechanism.


With all of this in mind, we will leave r:seq alone but will need to update r:quote, r:repeat, and r:alt as they all use r:seq without checking whether the expression will actually need to be wrapped. 
|#

;; helpers
(define (has-brackets? expr)
  (or
   (and (string=? "[" (string-head expr 1)) ; ERE
        (string=? "]" (string-tail expr (- (string-length expr) 1))))
   (and (string=? "\\[" (string-head expr 2)) ; BRE
        (string=? "\\]" (string-tail expr (- (string-length expr) 2))))))
   
(define (has-parentheses? expr)               
  (or           
   (and (string=? "(" (string-head expr 1)) ; ERE
        (string=? ")" (string-tail expr (- (string-length expr) 1))))
   (and (string=? "\\(" (string-head expr 2)) ; BRE
        (string=? "\\)" (string-tail expr (- (string-length expr) 2))))))

(define r:add-parentheses r:seq) ; for clarity in the future. also creating an add-parenteses-if-needed so either add-parentheses or add-parentheses-if-needed will replace r:seq

(define (r:add-parentheses-if-needed expr)
  (if (or (string=? expr "") ; r:alt may call this with an empty expression, so we need to be able to handle an empty expression
          (= (string-length expr) 1)
          (has-brackets? expr)
          (has-parentheses? expr))
      expr
      (r:add-parentheses expr)))

;; update/redefine functions
; r:repeat
(define (r:repeat min max expr)
  (string-append
   (r:add-parentheses-if-needed expr)
   "\\{"
   (number->string min)
   (if (number? max)
       (if (= max min)
           ""
           (string-append
            ","
            (number->string max)))
       ",")
   "\\}"))

; r:quote
(define (r:quote string)
  (r:add-parentheses-if-needed
   (list->string
    (append-map (lambda (char)
                  (if (memv char chars-needing-quoting)
                      (list #\\ char)
                      (list char)))
                (string->list string)))))

; r:alt
(define (r:alt . exprs)
  (if (pair? exprs)
      (let ((processed-exprs (map r:add-parentheses-if-needed exprs))) ; r:add-parentheses-if-needed does not take mutliple arguments, so we map one at a time instead
        (string-append (car processed-exprs)
                       (apply string-append
                              (append-map (lambda (expr)
                                            (list "\\|" expr))
                                          (cdr processed-exprs)))))
      (r:add-parentheses-if-needed "")))

;; test cases
; testing has-parentheses? and has-brackets? 
(has-parentheses? (r:add-parentheses "abc")) ; #t
(has-parentheses? "abc") ; #f
(has-brackets? "[abc]") ; #t
(has-brackets? "\\[abc\\]") ; #t
(has-brackets? "abc") ; #f
; testing r:add-parentheses-if-needed
(r:add-parentheses-if-needed "a") ; Value: "a"
(r:add-parentheses-if-needed "a.b") ; Value: "\\(a.b\\)"
(r:add-parentheses-if-needed "") ; Value: "" 
; testing r:repeat
(r:repeat 3 3 "a") ; Value: "a\\{3\\}"
(r:repeat 1 2 "ab") ; Value: "\\(ab\\)\\{1,2\\}"
(r:repeat 2 4 (r:alt "cat" "dog")) ; Value: "\\(cat\\)\\|\\(dog\\)\\{2,4\\}"
; testing r:quote
(display (r:seq (r:quote "a") (r:dot) (r:quote "c"))) ; fixed! now says \(a.c\) instead of \(\(a\).\(c\)\)
; testing r:alt
(r:alt "a" "b" "c") ; Value: "a\\|b\\|c"
(r:alt "ab" "cd" "ef") ; Value: "\\(ab\\)\\|\\(cd\\)\\|\\(ef\\)"



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;         PROBLEM 1.4      ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Q: Add in a procedure for constructing back-references.
(define (r:back-ref n)
  (if (and (integer? n) (> n 0))
      (string-append "\\" (number->string n))
      '"Bad back-reference"))

;; test cases
(r:back-ref 1) ; Value: "\\1"
(r:back-ref "n") ; Value: "Bad back-reference"
;; Confirming output from terminal matches
; grep '\(cat\)\1\(dog\)\2' Desktop/6.5151/ps01/code/tests.txt --> [10]. catcatdogdog [12]. catcatcatdogdogdog
(if (equal? (r:grep (string-append (r:quote "cat") (r:back-ref 1) (r:quote "dog") (r:back-ref 2)) "Desktop/6.5151/ps01/code/tests.txt")
            '("[10]. catcatdogdog" "[12]. catcatcatdogdogdog"))
    (display "PASS\n")
    (display "FAIL\n")) ; PASS



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;         PROBLEM 1.5      ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
Q:
a. What are the significant differences between BREs and EREs that make this a pain?  List the differences that must be addressed.

b. How can the back end be factored so that our language can compile into either kind of regular expression, depending on what is needed? How can we maintain the abstract layer that is independent of the target regular expression language?  Explain your strategy.

c. Extend our implementation to have both back ends.
|#

#|
The biggest difference is how each handles special characters. BRE requires a backslash before the `(`, `)`, `{`, `}`, and `|` characters. But, in scheme, we need to esacpe the backslash so while ERE uses the symbols plain, our backend processes BRE with `\\(`, `\\)`, `\\{`, `\\}`, and `\\|`. Rather than pass in an arguemnt saying ERE or BRE, I am going to define a global variable that will allow our system to change from basic to extended with one command. Also, not all of the functions need to be redone. For example, I wrote has-parentheses? and has-brackets? to handle both types.

We also have the case where ERE does not support back-references, so we can just add a message that the function will not work when attempting to use it with ERE.
|#

;;; r:add-parentheses
(define (r:add-parentheses . exprs)
  (if (string=? bas-or-ext "bas")
      (string-append "\\(" (apply string-append exprs) "\\)") ; if basic, add \\
      (string-append "(" (apply string-append exprs) ")")))
;; testing r:add-parentheses
(define bas-or-ext "bas")
(r:add-parentheses "abc") ; Value: "\\(abc\\)"
(define bas-or-ext "ext")
(r:add-parentheses "abc") ; Value: "(abc)"
; Note: add-parentheses-if-needed does not need to change since has-parentheses?, has-brackets?, and add-parentheses can handle ERE or BRE

;;; r:repeat
(define (r:repeat min max expr)
  (string-append
   (r:add-parentheses-if-needed expr)
   (if (string=? bas-or-ext "bas") ; check whether to add \\
       "\\{"
       "{")
   (number->string min)
   (if (number? max)
       (if (= max min)
           ""
           (string-append
            ","
            (number->string max)))
       ",")
   (if (string=? bas-or-ext "bas") ; check wheter to add \\
       "\\}"
       "}")))
;; testing r:repeat
(define bas-or-ext "bas")
(r:repeat 2 3 "cat") ; Value: "\\(cat\\)\\{2,3\\}"
(define bas-or-ext "ext")
(r:repeat 2 3 "cat") ; Value: "(cat){2,3}"

;;; r:alt
; r:alt
(define (r:alt . exprs)
  (if (pair? exprs)
      (let ((processed-exprs (map r:add-parentheses-if-needed exprs)))
        (string-append (car processed-exprs)
                       (apply string-append
                              (append-map (lambda (expr)
                                            (if (string=? bas-or-ext "bas")
                                                (list "\\|" expr)
                                                (list "|" expr)))
                                          (cdr processed-exprs)))))
      (r:add-parentheses-if-needed "")))
;; testing r:alt
(define bas-or-ext "bas")
(display (r:repeat 3 5 (r:alt "cat" "dog"))) ; \(cat\)\|\(dog\)\{3,5\} --> can run grep '\(cat\)\|\(dog\)\{3,5\}' Desktop/6.5151/ps01/code/tests.txt to get ("[09]. catdogcat" "[10]. catcatdogdog" "[11]. dogdogcatdogdog" "[12]. catcatcatdogdogdog" "[13]. acatdogdogcats" "[14]. ifacatdogdogs" "[15]. acatdogdogsme")
(define bas-or-ext "ext")
(display (r:repeat 3 5  (r:alt "cat" "dog"))) ; (cat)|(dog){3,5} --> can run grep -E '(cat)|(dog){3,5}' Desktop/6.5151/ps01/code/tests.txt to get ("[09]. catdogcat" "[10]. catcatdogdog" "[11]. dogdogcatdogdog" "[12]. catcatcatdogdogdog" "[13]. acatdogdogcats" "[14]. ifacatdogdogs" "[15]. acatdogdogsme") 

;;; r:back-ref
(define (r:back-ref n)
  (if (string=? bas-or-ext "bas")
      (if (and (integer? n) (> n 0))
          (string-append "\\" (number->string n))
          '"Bad back-reference")
      '"ERE doe not support back references"))
;; testing r:back-ref
(define bas-or-ext "bas")
(r:back-ref 1) ; Value: "\\1"
(define bas-or-ext "ext")
(r:back-ref 2) ; Value: "ERE doe not support back references"

;;; rewriting bourne-shell-grep-command-string so grep can run either ERE or BRE
(define (bourne-shell-grep-command-string expr filename)
  (string-append "grep " 
                 (if (string=? bas-or-ext "ext") "-E " "")  ; add `-E` if using EREs
                 "-e " (bourne-shell-quote-string expr) 
                 " " filename))
;; final testing!
; BRE!
(define bas-or-ext "bas")
(if (equal? (r:grep (r:repeat 3 5 (r:alt "cat" "dog")) "Desktop/6.5151/ps01/code/tests.txt")
    '("[09]. catdogcat" "[10]. catcatdogdog" "[11]. dogdogcatdogdog" "[12]. catcatcatdogdogdog" "[13]. acatdogdogcats" "[14]. ifacatdogdogs" "[15]. acatdogdogsme"))
    (display "PASS\n")
    (display "FAIL\n")) ; PASS
; ERE!
(define bas-or-ext "ext")
(if (equal? (r:grep (r:repeat 3 5 (r:alt "cat" "dog")) "Desktop/6.5151/ps01/co\de/tests.txt")
    '("[09]. catdogcat" "[10]. catcatdogdog" "[11]. dogdogcatdogdog" "[12]. catcatcatdogdogdog" "[13]. acatdogdogcats" "[14]. ifacatdogdogs" "[15]. acatdogdogsme"))
    (display "PASS\n")
    (display "FAIL\n")) ; PASS

#|
While the two test cases above look the same because we are just changing the global variable, it is testing that the outputs of the following produce the same thing:

   Terminal command 1: grep '\(cat\)\|\(dog\)\{3,5\}' Desktop/6.5151/ps01/code/tests.txt

   Terminal command 2: grep -E '(cat)|(dog){3,5}' Desktop/6.5151/ps01/code/tests.txt

At first, I had redefined all the functions to take in "ext" or "bas" as an argument, but this got clunky. After changing it to be globally defined, not only did it clean up sytax, it retained syntax, AND allowed me to change less procedures. 
|#
