;create-building.scm
;Create-building innehåller allt som byggnaderna behöver göra och veta.

;Tar in ett kort, med alla indata
;sedan ger kortet funktionen samt dess grundläggande egenskaper.
(define (create-card type name age science-cost cost production happiness culture science) 
  (let ((civilization #f)
        (workers 0)
        (tokens 0)
        (cost cost)
        (science-cost science-cost))         
    
    ;Tillför en arbetare till byggnaden.
    (define (add-worker self)
      (set! workers (+ workers 1)))  
    
    ;Tar bort en arbetare från byggnaden
    (define (remove-worker self)
      (if (not (= workers 0))
          (set! workers (- workers 1))
          (display "No workers present")))
    
    ;Skulle användas för senare kort.
    (define (adjust-cost self value)
      (if (< 0 (+ cost value))
          (set! cost (+ cost value))
          (set! cost 0)))
    
    ;Ber byggnaden att producera.
    (define (produce self)
      (ask civilization 'adjust 'culture (* workers culture))
      (ask civilization 'adjust 'science (* workers science))      
      (if (not (= production 0))
          (adjust-tokens self workers)))
    
    (define (adjust-tokens self value)
      (if (>= (ask civilization 'tokens) value)
          (begin (set! tokens (+ tokens value))
                 (ask civilization 'adjust 'tokens (- 0 value)))
          (adjust-tokens self (ask civilization 'tokens))))
    
    ;Summerar byggnadens attributer till civilizationen.     
    (define (sum-attribute self attribute)
      (cond ((eq? attribute 'culture) (* workers culture))
            ((eq? attribute 'happiness) (* workers happiness))
            ((eq? attribute 'production) (* workers production))
            ((eq? attribute 'science) (* workers science))))
    
    ;Summerar de resurser som byggnaden för tillfället innehar.
    (define (sum-resources self)
      (* tokens production))
    
    ;Säger till ett kort att tillhöra en viss civilization.
    (define (add-to-civilization self civilization-object)
      (set! civilization civilization-object))
    
    ;Kommandon som byggnaden kan ta emot.
    (define (self msg)
      (cond 
        ((eq? msg 'type) (lambda (self) type))
        ((eq? msg 'name) (lambda (self) name))
        ((eq? msg 'age) (lambda (self) age))
        ((eq? msg 'workers) (lambda (self) workers))
        ((eq? msg 'add-worker) add-worker)
        ((eq? msg 'remove-worker) remove-worker)
        ((eq? msg 'cost) (lambda (self) cost))
        ((eq? msg 'science) (lambda (self) science))
        ((eq? msg 'culture) (lambda (self) culture))
        ((eq? msg 'happiness) (lambda (self) happiness))
        ((eq? msg 'adjust-cost) adjust-cost)
        ((eq? msg 'produce) produce)
        ((eq? msg 'resources) sum-resources)
        ((eq? msg 'adjust-tokens) adjust-tokens)
        ((eq? msg 'science-cost) (lambda (self) science-cost))
        ((eq? msg 'sum-attribute) sum-attribute)
        ((eq? msg 'tokens) (lambda (self) tokens))
        ((eq? msg 'production) (lambda (self) production))
        ((eq? msg 'add-to-civilization) add-to-civilization)
        ((eq? msg 'civilization) (lambda (self) civilization))
        ))
    self))
