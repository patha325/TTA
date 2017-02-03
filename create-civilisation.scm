;create-civilisation.scm
;Create-civilization innehåller alla de primitiver 
;samt egenskaper som civilizationen behöver
;Här sparas och händer allt som civilizationen gör.

;Skriver ut fler än 1 element.
(define (display* . tokens)
  (for-each display tokens))

;Definerar alla startegenskaper en civilisation ska ha.
(define (create-civilisation color)
  (let ((culture 0)
        (science 0)
        (civil-actions 4)
        (military-actions 2)
        (government-list '())
        (mine-list '())
        (farm-list '())
        (temple-list '())
        (lab-list '())
        (arena-list '())
        (library-list '())
        (theater-list '())
        (tokens 18)
        (population-pool 18)
        (worker-pool 1)
        (civil-card-hand '()))
    
    ;väljer rätt byggnadslista beroende på byggnadstyp.
    (define (get-building-list building-type)
      (cond ((eq? building-type 'mine) mine-list)    
            ((eq? building-type 'farm) farm-list)     
            ((eq? building-type 'lab) lab-list)
            ((eq? building-type 'temple) temple-list)
            ((eq? building-type 'arena) arena-list)
            ((eq? building-type 'library) library-list)
            ((eq? building-type 'theater) theater-list)
            ((eq? building-type 'government) government-list)))
    
    ;Utför manuell ändring av vald parameter.    
    (define (adjust self identifier value)        
      (cond ((eq? identifier 'culture) (if (and (< value 0) (< culture (- value))) 
                                           (set! culture 0)
                                           (set! culture (+ culture value))))
            ((eq? identifier 'science) (if (and (< value 0) (< science (- value))) 
                                           (set! science 0)
                                           (set! science (+ science value))))
            ((eq? identifier 'civil-actions) (set! civil-actions (+ civil-actions value)))
            ((eq? identifier 'military-actions) (set! military-actions (+ military-actions value)))
            ((eq? identifier 'tokens) (set! tokens (+ tokens value)))
            ((eq? identifier 'minerals) (if (< value 0) 
                                            (if (enough? self 'minerals (- value))
                                                (remove-resources self 'minerals (- value))
                                                (display* "Not enough minerals!"))
                                            (add-resources self 'minerals value)))
            ((eq? identifier 'food) (if (< value 0)
                                        (if (enough? self 'food (- 0 value))
                                            (remove-resources self 'food (- value))
                                            (display* "Not enough food!"))
                                        (add-resources self 'food value)))
            ((eq? identifier 'worker-pool) (set! worker-pool (+ worker-pool value)
                                                 )))) 
    
    ;Lägger till resurser i produktionsbyggnaderna, innehåller även korten för senare åldar.
    (define (add-resources self resource-type value)
      (cond ((eq? resource-type 'minerals) (add-tokens-helper 'oil 'coal 'iron 'bronze mine-list value))
            ((eq? resource-type 'food) (add-tokens-helper 'mechanized-agriculture 'selective-breeding 'irrigation 'agriculture farm-list value))))
    (define (add-tokens-helper ageIII ageII ageI age0 building-list value)
      (cond       
        ((and (assq ageIII building-list) (<= 5 value)) (ask self 'building-command ageIII 'adjust-tokens 1)
                                                        (add-tokens-helper ageIII ageII ageI age0 building-list (- value 5)))
        ((and (assq ageII building-list) (<= 3 value)) (ask self 'building-command ageII 'adjust-tokens 1)
                                                       (add-tokens-helper ageIII ageII ageI age0 building-list (- value 3)))
        ((and (assq ageI building-list) (<= 2 value)) (ask self 'building-command ageI 'adjust-tokens 1)
                                                      (add-tokens-helper ageIII ageII ageI age0 building-list (- value 2)))
        ((and (assq age0 building-list) (<= 1 value)) (ask self 'building-command age0 'adjust-tokens 1)
                                                      (add-tokens-helper ageIII ageII ageI age0 building-list (- value 1)))))
    
    (define (remove-resources self resource-type value)
      (cond ((eq? resource-type 'minerals) (remove-tokens-helper 'minerals 'oil 'coal 'iron 'bronze mine-list value))
            ((eq? resource-type 'food) (remove-tokens-helper 'food 'mechanized-agriculture 'selective-breeding 'irrigation 'agriculture farm-list value))))
    
    ;Drar bort resurser från produktionsbyggnaderna, innehåller även korten för senare åldrar.
    (define (remove-tokens-helper resource-type ageIII ageII ageI age0 building-list value)
      (cond 
        ((and (< 0 value) (assq age0 building-list) (< 0 (ask self 'building-command age0 'tokens))) 
         (ask self 'building-command age0 'adjust-tokens -1)
         (remove-tokens-helper resource-type ageIII ageII ageI age0 building-list (- value 1)))
        
        ((and (< 0 value) (assq ageI building-list) (< 0 (ask self 'building-command ageI 'tokens))) 
         (ask self 'building-command ageI 'adjust-tokens -1)
         (remove-tokens-helper resource-type ageIII ageII ageI age0 building-list (- value 2)))
        
        ((and (< 0 value) (assq ageII building-list) (< 0 (ask self 'building-command ageII 'tokens))) 
         (ask self 'building-command ageII 'adjust-tokens -1)
         (remove-tokens-helper resource-type ageIII ageII ageI age0 building-list (- value 3)))
        
        ((and (< 0 value) (assq ageIII building-list) (< 0 (ask self 'building-command ageIII 'tokens))) 
         (ask self 'building-command ageIII 'adjust-tokens -1)
         (remove-tokens-helper resource-type ageIII ageII ageI age0 building-list (- value 5)))
        
        ((<= value 0) (add-resources self resource-type (- value)))))
    
    ;Lägger till en teknologi i civilizationen.
    (define (add-technology self tech-card)
      (if (and (enough? self 'science (ask tech-card 'science-cost))
               (< 0 civil-actions))
          (begin (set! civil-actions (- civil-actions 1))
                 (ask tech-card 'add-to-civilization self)
                 (set! science (- science (ask tech-card 'science-cost)))
                 (cond ((eq? 'mine (ask tech-card 'type)) (set! mine-list (cons (cons (ask tech-card 'name) tech-card) mine-list)))
                       ((eq? 'farm (ask tech-card 'type)) (set! farm-list (cons (cons (ask tech-card 'name) tech-card) farm-list)))
                       ((eq? 'temple (ask tech-card 'type)) (set! temple-list (cons (cons (ask tech-card 'name) tech-card) temple-list)))
                       ((eq? 'lab (ask tech-card 'type)) (set! lab-list (cons (cons (ask tech-card 'name) tech-card) lab-list)))
                       ((eq? 'arena (ask tech-card 'type)) (set! arena-list (cons (cons (ask tech-card 'name) tech-card) arena-list)))
                       ((eq? 'library (ask tech-card 'type)) (set! library-list (cons (cons (ask tech-card 'name) tech-card) library-list)))
                       ((eq? 'theater (ask tech-card 'type)) (set! theater-list (cons (cons (ask tech-card 'name) tech-card) theater-list)))
                       ((eq? 'government (ask tech-card 'type))
                        (extra-actions tech-card government-list)
                        (set! government-list (cons (cons (ask tech-card 'name) tech-card) government-list)))))
          #f))
    
    ;Tillför nya civil- och military-actions när ett nytt government kort spelats ut.
    (define (extra-actions tech-card tech-list)
      (if (and (not (empty? tech-list))
               (< 0 civil-actions))
          (begin (if (< 0 (- (ask tech-card 'civil-actions) (ask (cdr (car tech-list)) 'civil-actions)))
                     (set! civil-actions (+ civil-actions (- (ask tech-card 'civil-actions) (ask (cdr (car tech-list)) 'civil-actions)))))
                 (if (< 0 (- (ask tech-card 'military-actions) (ask (cdr (car tech-list)) 'military-actions)))
                     (set! military-actions (+ military-actions (- (ask tech-card 'military-actions) (ask (cdr (car tech-list)) 'military-actions))))))))
    
    ;Ger civilisationen ett nytt statsskick genom militär revolution.
    (define (revolution self government-card)
      (if (and (eq? civil-actions (ask self 'max-civil-actions))
               (enough? self 'science (ask government-card 'revolution-science-cost)))
          (begin (extra-actions government-card government-list)
                 (set! civil-actions 0)
                 (ask government-card 'add-to-civilization self)
                 (set! government-list (cons (cons (ask government-card 'name) government-card) government-list))
                 (set! science (- science (ask government-card 'revolution-science-cost))))
          #f))
    
    ;Uppgraderar en byggnad av lägre nivå till en högre. (Bronze -> Iron)
    (define (upgrade-building self original-building new-building)
      (if (and (< 0 civil-actions)
               (< 0 (ask self 'building-command original-building 'workers))
               (< (ask self 'building-command original-building 'age) (ask self 'building-command new-building 'age))
               (eq? (ask self 'building-command original-building 'type) (ask self 'building-command new-building 'type))
               (enough? self 'minerals (- (ask self 'building-command new-building 'cost) (ask self 'building-command original-building 'cost))))
          (begin (set! civil-actions (- civil-actions 1))
                 (ask self 'building-command original-building 'remove-worker)
                 (ask self 'building-command new-building 'add-worker)
                 (ask self 'adjust 'minerals (- (- (ask self 'building-command new-building 'cost) (ask self 'building-command original-building 'cost)))))
          #f))
    
    ;Visar namnen på samtliga kort civilisationen har på brädet. (testsyften)
    (define (display-played-cards self)
      (display-names (all-lists)))
    (define (display-names alist)
      (if (not (empty? alist)) 
          (begin (display (car (car alist)))
                 (newline)
                 (display-names (cdr alist)))))    
    
    ;Åkallar samtliga existerande byggnader av en viss kategori att utföra ett givet kommando.   
    (define (building-type-command self building-type command . args)
      (if (empty? args)
          (command-helper (get-building-list building-type) command)
          (command-helper (get-building-list building-type) command (car args))))
    
    ;Utför det givna kommandot till samtliga byggnader inom kategorin.
    (define (command-helper local-building-list command . args)     
      (if (not (empty? local-building-list))
          (if (empty? args)
              (begin 
                (ask (cdr (car local-building-list)) command)
                (command-helper (cdr local-building-list) command))
              (begin
                (ask (cdr (car local-building-list)) command (car args))
                (command-helper (cdr local-building-list command))))))
    
    ;Utför ett kommando på en specifik byggnad, åkallas med namn.    
    (define (building-command self building-name command . args)    
      (if (assq building-name (all-lists))
          (if (empty? args) 
              (ask (cdr (assq building-name (all-lists))) command)
              (ask (cdr (assq building-name (all-lists))) command (car args))
              )))
    (define (all-lists)
      (append mine-list farm-list temple-list lab-list arena-list library-list theater-list))
    (define (get-all-lists self)
      (all-lists))
    
    ;Skriver ut civilisationens nuvarande värden och resurser.    
    (define (status self)                                  
      (display* "Culture: " culture) (newline)
      (display* "Science: " science) (newline)
      (display* "Current civil actions: " civil-actions) (newline)
      (display* "Current military actions: " military-actions) (newline)                     
      (display* "Current minerals: " (ask self 'minerals)) (newline)
      (display* "Current food: " (ask self 'food)) (newline)
      (display* "Tokens in pool: " tokens) (newline)
      (display* "Population left: " population-pool) (newline)
      (display* "Workers ready for work: " worker-pool) (newline)
      )   
    
    ;Summerar produktionsbyggnaders nuvarande resurser. (farms eller mines)
    (define (sum-resources building-list)
      (if (empty? building-list) 0
          (+ (ask (cdr (car building-list)) 'resources) (sum-resources (cdr building-list)))))
    
    ;Kontrollerar att civilisationen har tillräckligt med resurser för det kommande kommandot.
    (define (enough? self attribute value)
      (cond ((eq? attribute 'minerals) (enough?-helper (ask self 'minerals) value))
            ((eq? attribute 'science) (enough?-helper science value))
            ((eq? attribute 'food) (enough?-helper (ask self 'food) value))))
    (define (enough?-helper attribute value)         
      (if (<= 0 (- attribute value)) #t #f))
    
    ;Drar bort resurser baserat på antalet tokens kvar i banken.
    (define (corruption self)
      (cond ((= tokens 0) (corruption-helper self 6))
            ((<= tokens 4) (corruption-helper self 4))
            ((<= tokens 8) (corruption-helper self 2))))
    (define (corruption-helper self corruption-amount)
      (if (enough? self 'minerals corruption-amount)
          (ask self 'adjust 'minerals (- corruption-amount))
          (ask self 'adjust 'minerals (- (ask self 'minerals)))))
    
    ;Drar bort mat beroende på antalet arbetare kvar i banken.   
    (define (consumption self)
      (cond ((= population-pool 0) (consumption-helper self 6))
            ((<= population-pool 4) (consumption-helper self 4))
            ((<= population-pool 8) (consumption-helper self 3))
            ((<= population-pool 12) (consumption-helper self 2))
            ((<= population-pool 16) (consumption-helper self 1))))
    (define (consumption-helper self consumption-amount)
      (if (enough? self 'food consumption-amount) 
          (ask self 'adjust 'food (- consumption-amount))
          (begin (ask self 'adjust 'culture (* 4 (- (ask self 'food) consumption-amount)))
                 (ask self 'adjust 'food (- (ask self 'food) consumption-amount)))))
    
    ;Ökar civilisationens population
    (define (increase-population self . discount)
      (cond ((= population-pool 0) (display "No workers remaining!!"))
            ((<= population-pool 4) (increase-population-helper  7 discount))
            ((<= population-pool 8) (increase-population-helper 5 discount))
            ((<= population-pool 12) (increase-population-helper 4 discount))
            ((<= population-pool 16) (increase-population-helper 3 discount))
            ((> population-pool 16) (increase-population-helper 2 discount))))
    (define (increase-population-helper cost discount)
      (if (null? discount)
          (if (and (enough? self 'food cost)
                   (< 0 civil-actions))
              (begin (set! civil-actions (- civil-actions 1))
                     (ask self 'adjust 'food (- cost))
                     (set! population-pool (- population-pool 1))
                     (set! worker-pool (+ worker-pool 1)))
              #f)
          (if (and (enough? self 'food (+ cost (car discount)))
                   (< 0 civil-actions))
              (begin (set! civil-actions (- civil-actions 1))
                     (ask self 'adjust 'food (- (- cost) (car discount)))
                     (set! population-pool (- population-pool 1))
                     (set! worker-pool (+ worker-pool 1)))
              #f)))
    
    ;Bygger en ny byggnad. 
    (define (build-new self building-name)
      (if (and (> worker-pool 0) 
               (enough? self 'minerals (ask self 'building-command building-name 'cost))
               (building-limit-ok? self building-name)
               (< 0 civil-actions))
          (begin (set! civil-actions (- civil-actions 1))
                 (ask self 'building-command building-name 'add-worker)
                 (set! worker-pool (- worker-pool 1))
                 (ask self 'adjust 'minerals (- (ask self 'building-command building-name 'cost))))
          #f))
    
    ;Kontrollerar att det är tillåtet att bygga byggnaden med avsikt på building-limit.
    (define (building-limit-ok? self building-name)
      (if (or 
           (or
            (eq? 'mine (ask self 'building-command building-name 'type))
            (eq? 'farm (ask self 'building-command building-name 'type)))
           (and
            (or (eq? 'temple (ask self 'building-command building-name 'type))
                (eq? 'lab (ask self 'building-command building-name 'type))
                (eq? 'arena (ask self 'building-command building-name 'type))
                (eq? 'theater (ask self 'building-command building-name 'type))
                (eq? 'library (ask self 'building-command building-name 'type)))
            (< (ask self 'building-command building-name 'workers) (ask (cdr (car government-list)) 'building-limit))))
          #t
          #f))
    
    ;Förstör en av civilisationens byggnader, och återför byggnadens arbetare till worker-pool
    (define (destroy self building-name)
      (if (and (< 0 civil-actions)
               (< 0 (ask self 'building-command building-name 'workers)))
          (begin (ask self 'building-command building-name 'remove-worker)
                 (set! civil-actions (- civil-actions 1))
                 (set! worker-pool (+ worker-pool 1)))
          #f))
    
    ;Fyller på civil- och military-actions till civilisationsmax.
    (define (restock-actions self)
      (set! civil-actions (ask (cdr (car government-list)) 'civil-actions))
      (set! military-actions (ask (cdr (car government-list)) 'military-actions)))
    
    ;Slutför rundan, ber byggnaderna producera och ger nya actions.
    (define (end-of-turn self)
      (if (ask self 'happy?)
          (begin (ask self 'building-type-command 'farm 'produce)       
                 (ask self 'building-type-command 'mine 'produce)
                 (ask self 'building-type-command 'lab 'produce)
                 (ask self 'building-type-command 'theater 'produce)
                 (ask self 'building-type-command 'temple 'produce)
                 (ask self 'building-type-command 'library 'produce)))
      (ask self 'consumption)  
      (ask self 'corruption)
      (ask self 'restock-actions))
    
    ;Anger det maximala antalet civil-actions civilisationen för tillfället kan ha.
    (define (max-civil-actions self)
      (ask (cdr (car government-list)) 'civil-actions))  
    
    ;Lägger till ett kort till civilisationshanden.
    (define (pick-card self card-object position)
      (if (and (< (length civil-card-hand) (ask self 'max-civil-actions))
               (<= (ask self 'civil-card-cost position) civil-actions))
          (begin
            (set! civil-card-hand (cons (cons (ask card-object 'name) card-object) civil-card-hand))
            (set! civil-actions (- civil-actions (ask self 'civil-card-cost position))))
          #f))
    
    ;Kontrollerar kostnaden för ett kort på brädet.
    (define (civil-card-cost self position)  
      (cond ((<= 10 position) 3)
            ((<= 6 position) 2)
            ((<= 1 position) 1)))
    
    ;Ökar mängden science som civilizationen har, med avseend på science som produceras.
    (define (score-science self)
      (set! science (+ science (ask self 'sum/turn 'science))))
    
    ;Ökar mängden culture som civilizationen har, med avseend på culture som produceras.
    (define (score-culture self)
      (set! culture (+ culture (ask self 'sum/turn 'culture))))
    
    ;Kontrollerar att civilizationen inte är olycklig (producerar då inget).
    (define (happy? self)
      (cond ((= population-pool 0) (happy-helper 8))
            ((<= population-pool 2) (happy-helper 7))
            ((<= population-pool 4) (happy-helper 6))
            ((<= population-pool 6) (happy-helper 5))
            ((<= population-pool 8) (happy-helper 4))
            ((<= population-pool 10) (happy-helper 3))
            ((<= population-pool 12) (happy-helper 2))
            ((<= population-pool 16) (happy-helper 1))
            ((>= population-pool 18) #t)))
    
    (define (happy-helper happiness-required)
      (if (>= (+ (ask self 'sum/turn 'happiness) worker-pool) happiness-required)
          #t
          #f))
    
    ;Summerar vald parameter, ser hur mycket av den som produceras per runda.
    (define (sum/turn self parameter)
      (cond ((eq? parameter 'food)
             (sum-help 'production farm-list))
            ((eq? parameter 'minerals)
             (sum-help 'production mine-list))
            ((eq? parameter 'science)
             (+ (sum-help 'science lab-list)
                (sum-help 'science library-list)))
            ((eq? parameter 'culture)
             (+ (ask (cdar government-list) 'culture)
                (sum-help 'culture theater-list)
                (sum-help 'culture library-list)
                (sum-help 'culture temple-list)))
            ((eq? parameter 'happiness)
             (+ (ask (cdar government-list)  'happiness)
                (sum-help 'happiness theater-list)
                (sum-help 'happiness arena-list)
                (sum-help 'happiness temple-list)))))
    
    (define (sum-help parameter building-list)
      (if (empty? building-list)
          0
          (+ (* (ask (cdr (car building-list)) 'workers) (ask (cdr (car building-list)) parameter))
             (sum-help parameter (cdr building-list)))))
    
    ;Tar bort ett kort från civilisationshanden.
    (define (drop-card self position)
      (set! civil-card-hand (drop-help civil-card-hand position)))
    (define (drop-help card-hand position)
      (if (= position 1) 
          (cdr card-hand)
          (cons (car card-hand) (drop-help (cdr card-hand) (- position 1)))))
    
    (define (card-exists? self card-object)
      (if (or (assq (ask card-object 'name) (all-lists))
              (assq (ask card-object 'name) civil-card-hand))
          #t #f))
    
    ;Kommandon som civilizationen kan hantera. 
    (define (self msg)
      (cond
        ((eq? msg 'add-technology) add-technology)
        ((eq? msg 'revolution) revolution)
        ((eq? msg 'display-played-cards) display-played-cards)
        ((eq? msg 'building-type-command) building-type-command)
        ((eq? msg 'building-command) building-command)
        ((eq? msg 'status) status)
        ((eq? msg 'adjust) adjust)
        ((eq? msg 'enough?) enough?)
        ((eq? msg 'tokens) (lambda (self) tokens))
        ((eq? msg 'corruption) corruption)
        ((eq? msg 'consumption) consumption)
        ((eq? msg 'minerals) (lambda (self) (sum-resources mine-list)))
        ((eq? msg 'food) (lambda (self) (sum-resources farm-list)))
        ((eq? msg 'increase-population) increase-population)
        ((eq? msg 'build-new) build-new)
        ((eq? msg 'destroy) destroy)
        ((eq? msg 'restock-actions) restock-actions)
        ((eq? msg 'end-of-turn) end-of-turn)
        ((eq? msg 'worker-pool) (lambda (self) worker-pool))
        ((eq? msg 'population-pool) (lambda (self) population-pool))
        ((eq? msg 'get-all-lists) get-all-lists)
        ((eq? msg 'upgrade-building) upgrade-building)
        ((eq? msg 'civil-actions) (lambda (self) civil-actions))
        ((eq? msg 'military-actions) (lambda (self) military-actions))
        ((eq? msg 'science) (lambda (self) science))
        ((eq? msg 'culture) (lambda (self) culture))
        ((eq? msg 'max-civil-actions) max-civil-actions)
        ((eq? msg 'pick-card) pick-card)
        ((eq? msg 'civil-card-cost) civil-card-cost)
        ((eq? msg 'civil-card-hand) (lambda (self) civil-card-hand))
        ((eq? msg 'score-science) score-science)
        ((eq? msg 'score-culture) score-culture)
        ((eq? msg 'happy?) happy?)
        ((eq? msg 'sum/turn) sum/turn)
        ((eq? msg 'current-government) (lambda (self) (car (car government-list))))
        ((eq? msg 'drop-card) drop-card)
        ((eq? msg 'card-exists?) card-exists?)
        ))
    self))



