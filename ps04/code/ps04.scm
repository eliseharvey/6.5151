#|
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

Note: I changed the actual files adventure-object and adventure-world. I will include a copy of the code changed below.
|#


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
    (if (< current-health 10)
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

#|
I want to set it up so each person starts with $5 when initialized. I will place vending machines in lobby 7 and 10. Then, if there is a vending machine, a person can buy 1 unit of health (out of 10) for $1 using a function called pay! This function will need to check if there is a vending machine in the location, the person's balance, and the person's health. I am going to assume that you cannot exceep 100% health. Lastly, I want each person to accrue money at an interest rate of 10% per tick.
|#

;; add a wallet and functionality to access it
(define person:wallet
  (make-property 'wallet
                 'predicate n:real?
                 'default-value 5)) 

;; add to person
(define person?
  (make-type 'person (list person:health person:bag person:wallet))) ;; Add wallet property
(set-predicate<=! person? mobile-thing?)

;; getter and setter
(define get-wallet
  (property-getter person:wallet person?))
(define set-wallet!
  (property-setter person:wallet person? n:real?))

;; accrue interest
(define (accrue-interest! person)
  (let ((current-money (get-wallet person)))
    (set-wallet! person (+ current-money 0.1))))
(define-clock-handler person? accrue-interest!)

;; add vending machines
(create-thing 'vending-machine lobby-7)
(create-thing 'vending-machine lobby-10)

;; create the buy-food! function
(define (buy-food! person amount)
  (let ((location (get-location person))
        (current-money (get-wallet person))
        (current-health (get-health person)))
    ; confirm a vending machine is at the location
    (if (not (find-object-by-name 'vending-machine (get-things location)))
        (announce! (list "There are no vending machine here!"))
        ; make sure not already at full health
        (if (>= current-health 10)
            (announce! (list "Silly goose! You are at full health, you don't need food!"))
            ; if can buy foiod, see how much (not exceeding 10)
            (let* ((max-affordable (floor (/ current-money 1)))
                   (max-needed (- 10 current-health))  
                   (actual-purchase (min amount max-affordable max-needed))  
                   (cost (* actual-purchase 1))  
                   (remaining-money (- current-money cost)))  
              ; narrate adjsutment if needed
              (if (< actual-purchase amount)
                  (announce! (list "Vending machine: You couldn't buy" amount "units, but you bought" actual-purchase "instead."))) 
              ; update wallet and health
              (set-wallet! person remaining-money)
              (set-health! person (+ current-health actual-purchase))
              ; announce the transaction
              (announce! (list "Vending machine: You are healthier! Your health is now" (get-health person) "and your new balance is $" remaining-money "!")))))))

#|
(buy-food! my-avatar 1)
Silly goose! You are at full health, you don't need food!
.
.
.
(buy-food! my-avatar 9)
There are no vending machine here!
.
.
.
(buy-food! my-avatar 200)
Vending machine: You couldn't buy 200 units, but you bought 5 instead.
Vending machine: You are healthier! Your health is now 6 and your new balance is $ 0 !
|#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;         PROBLEM 3.21     ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
I think it would be cool if we can fight a troll or other people. If fighting a troll, you have a 50% chance of winning. If you win, the troll dies and youy lose 10% of health. If you lose, nothing happens to the troll and you die.

If you fight another person in the world, then it comes down to has the higher health. The person with higher health wins and again loses 10% of health. If they have the same health, they both lose 10%. The loser will die. To make this incentive, the winner will also rob the loser and take their wallet. 
|#

(define (fight! attacker defender-name)
  ; get the defender (object NOT just anme)
  (let ((defender (find-object-by-name defender-name (people-here attacker))))
    (if (not defender)
        (announce! (list "There is no one named" defender-name "here to fight!"))
        (if (not (and (person? attacker) (or (person? defender) (troll? defender))))
            (announce! (list "You can't fight that!"))
            ; fighting a troll
            (if (troll? defender)
                (if (flip-coin 0.5)  ; 50% chance 
                    (begin
                      (say! attacker (list "I defeated" (get-name defender) "!"))
                      (suffer! 1 attacker)  
                      (announce! (list (get-name defender) "lets out a horrible roar and collapses!"))
                      (move! defender (get-heaven) defender))  ; move troll to heaven?
                    ; troll wins
                    (begin
                      (say! defender (list "Hah! I win, " (get-name attacker) " is finished!"))
                      (die! attacker)))
                ; fighting a persom
                (let ((attacker-health (get-health attacker))
                      (defender-health (get-health defender))
                      (attacker-money (get-wallet attacker))
                      (defender-money (get-wallet defender)))
                  (cond 
                    ; attacker wins
                    ((> attacker-health defender-health)
                     (say! attacker (list "I defeated" (get-name defender) "!"))
                     (suffer! 1 attacker)
                     (set-wallet! attacker (+ attacker-money defender-money))
                     (set-wallet! defender 0)  
                     (say! attacker (list "I took all of" (get-name defender) "'s money! I now have $" (get-wallet attacker) "!"))
                     (die! defender))
                    ; defender wins
                    ((< attacker-health defender-health)
                     (say! defender (list "I defeated" (get-name attacker) "!"))
                     (suffer! 1 defender)
                     (set-wallet! defender (+ defender-money attacker-money))  
                     (set-wallet! attacker 0) 
                     (say! defender (list "I took all of" (get-name attacker) "'s money! I now have $" (get-wallet defender) "!"))
                     (die! attacker))
                    ; they tie
                    (else
                     (announce! (list "It's a fair fight! You both take damage."))
                     (suffer! 1 attacker)
                     (suffer! 1 defender)))))))))

#|
(fight! my-avatar 'grendel)
Elise says: I defeated grendel !
Elise says: Ouch! 1 hits is more than I want!
grendel lets out a horrible roar and collapses!
.
.
.
(fight! my-avatar 'course-6-frosh)
course-6-frosh says: I defeated Elise !
course-6-frosh says: Ouch! 1 hits is more than I want!
course-6-frosh says: I took all of Elise 's money! I now have $ 10.699999999999998 !
An earth-shattering, soul-piercing scream is heard...
Elise enters heaven
.
.
.

(fight! my-avatar 'dr-evil)
It's a fair fight! You both take damage.
Elise says: Ouch! 1 hits is more than I want!
dr-evil says: Ouch! 1 hits is more than I want!

(buy-food! my-avatar 1)
Vending machine: You are healthier! Your health is now 10. and your new balance is $ 20.399999999999977 !

(fight! my-avatar 'dr-evil)
Elise says: I defeated dr-evil !
Elise says: Ouch! 1 hits is more than I want!
Elise says: I took all of dr-evil 's money! I now have $ 29.499999999999964 !
An earth-shattering, soul-piercing scream is heard...
|#

#|
I had an absurd amount of fun going around and fighting people. Even though it doesn't show in the examples I added, I actually lost a lot haha. This works really well! I may go back and update how the money collects interest, because the decimals are a bit annoying. But this would entail scaling the vending machine amounts like I did health, which would make the prices a little unrealistic, so I am not mad about keeping it this way.
|#
