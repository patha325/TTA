;board.scm
;skapar brädet som har hand om kortlekarna och kortraden under spelets gång.

(define board
  (let ((card-row '())
        (civil-deck '())
        (military-deck '())
        (current-events '()))

    ;Tar bort tomma element ur kortraden.
    (define (drop-down-cards self)
      (set! card-row (card-drop-assist card-row)))
    (define (card-drop-assist card-row)
      (cond ((empty? card-row) '())
            ((empty? (car card-row)) (card-drop-assist (cdr card-row)))
            ((not (empty? card-row)) (cons (car card-row) (card-drop-assist (cdr card-row))))
            ))
              
    ;Tar bort ett kortobjekt från kortraden.
    (define (remove-card self card-object)
      (set! card-row (remove-card-assist card-object card-row)))
    (define (remove-card-assist card-object card-row)
      (if (equal? card-object (car card-row))
          (cons '() (cdr card-row))
          (cons (car card-row) (remove-card-assist card-object (cdr card-row)))))
          
    ;Åkallar de tre funktioner som tar bort kort, tömmer kortraden på tomma element och fyller på kortraden.
    (define (new-turn self)
      (begin
        (new-card-phase)
        (drop-down-cards self)
        (add-cards-to-row self)))
    
    ;Tar bort 1-3 av de nedersta korten beroende på antalet spelare, nuvarande 1 kort/runda.
    (define (new-card-phase)
      (cond 
        ((not (empty? (car card-row))) (begin
                                         (remove-card self (car card-row))
        ;                                 (new-card-phase)))
        ;((not (empty? (cadr card-row))) (begin
        ;                                  (remove-card self (cadr card-row))
        ;                                  (new-card-phase)))
        ;((not (empty? (caddr card-row))) (remove-card self (caddr card-row)))
        ))))
    
    ;Fyller på kortraden upp till 13 kort.
    (define (add-cards-to-row self)
      (add-cards-setter (car (add-cards-helper (reverse card-row) civil-deck))
                        (cdr (add-cards-helper (reverse card-row) civil-deck))))
    
    (define (add-cards-setter new-card-row new-civil-deck)
      (set! card-row new-card-row)
      (set! civil-deck new-civil-deck))    
    
    (define (add-cards-helper card-row civil-deck)
      (if (and (not (empty? civil-deck)) (< (length card-row) 13))
          (add-cards-helper (cons (car civil-deck) card-row) (cdr civil-deck))
          (cons (reverse card-row) civil-deck)))
  
    ;Ger brädet en ny kortlek.
    (define (new-deck self type deck)
      (cond ((eq? type 'civil) (set! civil-deck deck))
            ((eq? type 'military) (set! military-deck deck))))
    
    ;Hämtar positionen av ett kortobjekt i kortraden.
    (define (card-position self card-object)
      (card-pos-helper card-object card-row 0))
    (define (card-pos-helper card-object card-row index)
      (if (equal? card-object (car card-row)) index
          (card-pos-helper card-object (cdr card-row) (+ index 1))))
    
    ;Hämtar ett kortobjekt från dess position i kortraden.
    (define (get-card self position)
      (list-ref card-row (- position 1)))
    
    ;Hanterar anrop.
    (define (self msg)
      (cond ((eq? msg 'drop-down-cards) drop-down-cards)
            ((eq? msg 'new-deck) new-deck)
            ((eq? msg 'remove-card) remove-card)
            ((eq? msg 'civil-deck) (lambda (self) civil-deck))
            ((eq? msg 'card-row) (lambda (self) card-row))
            ((eq? msg 'new-turn) new-turn)
            ((eq? msg 'add-cards-to-row) add-cards-to-row)
            ((eq? msg 'card-position) card-position)
            ((eq? msg 'handle-card) handle-card)
            ((eq? msg 'get-card) get-card)
            ((eq? msg 'initiate-deck) initiate-deck)
            ))
    self))

;Blandar en lista (kortlek).

(define (shuffle deck)
  (if (empty? deck)
      '()
      (let ((index (random (length deck))))
        (cons (list-ref deck index)
        (shuffle (list-remove deck index))))))
(define (list-remove deck index)
  (if (= 0 index)
      (cdr deck)
      (cons (car deck) (list-remove (cdr deck) (- index 1)))))

;Skapar en lek från valfritt antal argument.
(define (create-deck . args)
  args)
  



