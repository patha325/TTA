;special-cards.scm
;Skapar strukturen för samtliga specialkort.

(define (create-special-card name age)
  (let ((playable #f))
    
    (define (self msg)
      (cond ((eq? msg 'playable?) (lambda (self) playable))
            ((eq? msg 'set-playable) (lambda (self) (set! playable #t)))
            ((eq? msg 'name) (lambda (self) name))
            ((eq? msg 'age) (lambda (self) age))
            ((eq? msg 'type) (lambda (self) 'special))))
    self))

