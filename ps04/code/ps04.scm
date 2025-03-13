A#|
Elise Harvey
Kerb: erharvey
6.5151 ps04
|#

(load "~/Desktop/6.5151/sdf/manager/load.scm")
(manage 'new 'user-defined-types)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;         PROBLEM 3.16     ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(start-adventure "Elise")
You are in lobby-7 
You can see: lobby-10 infinite-corridor 
You can exit: up west east

(Go ‘up)
Elise leaves via the up exit
Elise enters little-dome
You are in little-dome 
You can see: great-dome
You can exit: down

(go 'down)
Elise leaves via the down exit
Elise enters lobby-7
Elise says: Hi registrar 
You are in lobby-7 
You see here: registrar
You can see: lobby-10 infinite-corridor 
You can exit: up west east  
.
.
.
I kept playing until I died
|#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;         PROBLEM 3.17     ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
It looks like when we start the game, our avatar has a health of 3. Once that health drops to 0 (two troll bites), we die. A troll attacks with a coin flip. I want to scale health to be [0, 1] and initialized at full health (1 meaning 100% health). I don't really want to deal with floating point or roundoff errors, so I am going to scale this to be [0, 10]. Additionally, we will implement a health growth system that increases health by 1 with each tick, but iff health is less than 10 (you cannot exceed 100% health).

Note: I changed the actual files adventure-objects, adventure-substrate, and adventure-world. I will include a copy of the code changed below.
|#


;;; adventure-objects
;; health gets initialized to 10 instead of 3
(define person:health
  (make-property 'health
                 'predicate n:exact-integer?
                 'default-value 10))
;; when a troll bites a person, it can be a random number in 10
(define (eat-people! troll)
  (if (flip-coin (get-hunger troll))
      (let ((people (people-here troll)))
        (if (n:null? people)
            (narrate! (list (possessive troll) "belly rumbles")
                      troll)
            (let ((victim (random-choice people)))
              (narrate! (list troll "takes a bite out of" victim)
                        troll)
              (suffer! (random-number 10) victim))))))
;; healing a person with each clock tick!
(define (heal-person! person)
  (let ((current-health (get-health person)))
    (if (< current-health 3)
        (begin
          (set-health! person (+ current-health 1))
          (say! person (list "I am healing! I can now take" (get-health person) "bites!"))))))
(define-clock-handler person? heal-person!)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;         PROBLEM 3.18     ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
For this, I am going to make MIT Medical as some entity that is above the green building but and south of the gates tower. I need to add it as a place, add the ways to exit, add line-of-sight, AND add the healing mechanism.
|#

;; added the following line in create-mit
(medical (create-place 'medical))
(can-go-both-ways bldg-54 'up 'down medical)
(can-go-both-ways medical 'north 'south 32G)
(can-see-both-ways bldg-54 medical)
(can-see-both-ways 32G medical) 
;; ALSO added medical to the list at the bottom
(define-generic-procedure-handler enter-place!
  (match-args person?)
  (lambda (super person)
    (super person)
    (narrate! (list person "enters" (get-location person))
              person)
    (let ((people (people-here person)))
      (if (n:pair? people)
          (say! person (cons "Hi" people)))) 
    ;; ADDED healing if in medical  
    (if (eq? (get-location person) (find-object-by-name 'medical (get-all-places)))
        (let ((person-health (get-health person)))  
          (if (< person-health 10)  
              (begin  
                (set-health! person 10)   
                (say! person (list "I am fully healed!")))  
              (say! person (list "I am fully healthy already! No healing is needed.")))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;       FOOD FOR HEALTH    ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO
