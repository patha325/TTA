;create-technology.scm
;Skapar alla teknologier - såsom statsskick, lagar och strategi.

(define (create-technology age type name science-cost revolution-science-cost civil-actions military-actions building-limit happiness culture strength colonization)
  (let ((civilization #f))
    
    ;Tillför teknologin till en civilization.
    (define (add-to-civilization self civilization-object)
      (set! civilization civilization-object))
    
    ;De kommandon som teknologin kan ta emot.
    (define (self msg)
      (cond
        ((eq? msg 'age) (lambda (self) age))
        ((eq? msg 'type) (lambda (self) type))
        ((eq? msg 'name) (lambda (self) name))
        ((eq? msg 'science-cost) (lambda (self) science-cost))
        ((eq? msg 'revolution-science-cost) (lambda (self) revolution-science-cost))
        ((eq? msg 'civil-actions) (lambda (self) civil-actions))
        ((eq? msg 'military-actions) (lambda (self) military-actions))
        ((eq? msg 'building-limit) (lambda (self) building-limit))
        ((eq? msg 'happiness) (lambda (self) happiness))
        ((eq? msg 'culture) (lambda (self) culture))
        ((eq? msg 'strength) (lambda (self) strength))
        ((eq? msg 'colonization) (lambda (self) colonization))
      ((eq? msg 'add-to-civilization) add-to-civilization)))
  self))