;Blue-civ.scm
;Blue-civ innehåller allt grafiskt gällande den blå civilizationen.

;Strukturer.

(define canvas-frame (instantiate frame% ("Blue Civilization")))
(define card-panel (instantiate horizontal-panel% (canvas-frame)))
(define theater-pane (instantiate vertical-pane% (card-panel)))
(define arena-pane (instantiate vertical-pane% (card-panel)))
(define library-pane (instantiate vertical-pane% (card-panel)))
(define lab-pane (instantiate vertical-pane% (card-panel)))
(define temple-pane (instantiate vertical-pane% (card-panel)))
(define farm-pane (instantiate vertical-pane% (card-panel)))
(define mine-pane (instantiate vertical-pane% (card-panel)))
(define canvas-pane (instantiate vertical-pane% (canvas-frame)))
(define sideways-panel (instantiate horizontal-panel% (canvas-frame)))

(instantiate button% ("End Turn" canvas-pane (lambda (button event)
                                               (if (eq? (ask main 'current-civ) blue-civ)
                                                   (ask main 'end-turn)))))
;Utritning.

(define blue-civ-canvas%
  (class canvas%
    (override on-event)
    (define on-event (lambda (event) (begin 
                                       (if (and (send event button-up? 'left)
                                                (eq? blue-civ (ask main 'current-civ))
                                                (> (send event get-x) 450)
                                                (< (send event get-x) 515)
                                                (> (send event get-y) 20)
                                                (< (send event get-y) 85))
                                           (begin (ask main 'handle 'worker-pool)
                                                  (update-civilization blue-civ)
                                                  (click-marker blue-civ-canvas)))
                                       (if (and (send event button-up? 'left)
                                                (eq? blue-civ (ask main 'current-civ))
                                                (> (send event get-x) 5)
                                                (< (send event get-x) 520)
                                                (> (send event get-y) 90)
                                                (< (send event get-y) 180))
                                           (begin (ask main 'handle 'population-pool)
                                                  (update-civilization blue-civ)
                                                  (click-marker blue-civ-canvas))))))
    
    (super-instantiate ())))

(define (draw-civilization canvas dc)
  (send dc clear)
  (send dc set-background (make-object color% 255 255 255))
  (send dc draw-bitmap blue-civilization 0 0)
  (update-civilization blue-civ))

(define blue-civ-canvas
  (instantiate blue-civ-canvas% ()
    (parent sideways-panel)
    (paint-callback draw-civilization)
    (min-height 190)
    (min-width 530)
    (stretchable-width #f) 
    (stretchable-height #f)))

;Kortdefiniering och utritning.

(define canvas-height 134)
(define canvas-width 89)

;bronze

(define bronze-canvas%
  (class canvas%
    (override on-event)
    (define on-event (lambda (event) (if (send event button-up? 'left)                                                
                                         (operate-card 'bronze bronze-canvas bronze-gui blue-civ))))
    (super-instantiate ())))

(define (draw-bronze canvas dc)
  (send dc clear)
  (update-caller 'bronze blue-civ) 
  (click-marker canvas))

(define bronze-canvas
  (instantiate bronze-canvas% ()
    (parent mine-pane)
    (paint-callback draw-bronze)
    (min-height canvas-height)
    (min-width canvas-width)
    (stretchable-width #f) 
    (stretchable-height #f)))

;iron

(define iron-canvas%
  (class canvas%
    (override on-event)
    (define on-event (lambda (event) (if (send event button-up? 'left)                                                
                                         (operate-card 'iron iron-canvas iron-gui blue-civ))))
    (super-instantiate ())))

(define (draw-iron canvas dc)
  (send dc clear)
  (update-caller 'iron blue-civ)
  (click-marker canvas))

(define iron-canvas
  (instantiate iron-canvas% ()
    (parent mine-pane)
    (style '(deleted))
    (paint-callback draw-iron)
    (min-height canvas-height)
    (min-width canvas-width)
    (stretchable-width #f) 
    (stretchable-height #f)))

;agriculture

(define agriculture-canvas%
  (class canvas%
    (override on-event)
    (define on-event (lambda (event) (if (send event button-up? 'left)                                                
                                         (operate-card 'agriculture agriculture-canvas agriculture-gui blue-civ))))
    (super-instantiate ())))

(define (draw-agriculture canvas dc)
  (send dc clear)
  (update-caller 'agriculture blue-civ)
  (click-marker canvas))


(define agriculture-canvas
  (instantiate agriculture-canvas% ()
    (parent farm-pane)
    (paint-callback draw-agriculture)
    (min-height canvas-height)
    (min-width canvas-width)
    (stretchable-width #f) 
    (stretchable-height #f)))

;irrigation


(define irrigation-canvas%
  (class canvas%
    (override on-event)
    (define on-event (lambda (event) (if (send event button-up? 'left)                                                
                                         (operate-card 'irrigation irrigation-canvas irrigation-gui blue-civ))))
    (super-instantiate ())))

(define (draw-irrigation canvas dc)
  (send dc clear)
  (update-caller 'irrigation blue-civ)
  (click-marker canvas))

(define irrigation-canvas
  (instantiate irrigation-canvas% ()
    (parent farm-pane)
    (style '(deleted))
    (paint-callback draw-irrigation)
    (min-height canvas-height)
    (min-width canvas-width)
    (stretchable-width #f) 
    (stretchable-height #f)))

;philosophy

(define philosophy-canvas%
  (class canvas%
    (override on-event)
    (define on-event (lambda (event) (if (send event button-up? 'left)                                                
                                         (operate-card 'philosophy philosophy-canvas philosophy-gui blue-civ))))
    (super-instantiate ())))

(define (draw-philosophy canvas dc)
  (send dc clear)
  (update-caller 'philosophy blue-civ)
  (click-marker canvas))

(define philosophy-canvas
  (instantiate philosophy-canvas% ()
    (parent lab-pane)
    (paint-callback draw-philosophy)
    (min-height canvas-height)
    (min-width canvas-width)
    (stretchable-width #f) 
    (stretchable-height #f)))

;religion

(define religion-canvas%
  (class canvas%
    (override on-event)
    (define on-event (lambda (event) (if (send event button-up? 'left)                                                
                                         (operate-card 'religion religion-canvas religion-gui blue-civ))))
    (super-instantiate ())))

(define (draw-religion canvas dc)
  (send dc clear)
  (update-caller 'religion blue-civ)
  (click-marker canvas))

(define religion-canvas
  (instantiate religion-canvas% ()
    (parent temple-pane)
    (paint-callback draw-religion)
    (min-height canvas-height)
    (min-width canvas-width)
    (stretchable-width #f) 
    (stretchable-height #f)))

;printing press

(define printing-press-canvas%
  (class canvas%
    (override on-event)
    (define on-event (lambda (event) (if (send event button-up? 'left)                                                
                                         (operate-card 'printing-press printing-press-canvas printing-press-gui blue-civ))))
    (super-instantiate ())))

(define (draw-printing-press canvas dc)
  (send dc clear)
  (update-caller 'printing-press blue-civ)
  (click-marker canvas))

(define printing-press-canvas
  (instantiate printing-press-canvas% ()
    (parent library-pane)
    (style '(deleted))
    (paint-callback draw-printing-press)
    (min-height canvas-height)
    (min-width canvas-width)
    (stretchable-width #f) 
    (stretchable-height #f)))

;alchemy

(define alchemy-canvas%
  (class canvas%
    (override on-event)
    (define on-event (lambda (event) (if (send event button-up? 'left)                                                
                                         (operate-card 'alchemy alchemy-canvas alchemy-gui blue-civ))))
    (super-instantiate ())))

(define (draw-alchemy canvas dc)
  (send dc clear)
  (update-caller 'alchemy blue-civ)
  (click-marker canvas))

(define alchemy-canvas
  (instantiate alchemy-canvas% ()
    (parent lab-pane)
    (style '(deleted))
    (paint-callback draw-alchemy)
    (min-height canvas-height)
    (min-width canvas-width)
    (stretchable-width #f) 
    (stretchable-height #f)))

;bread-and-circuses

(define bread-and-circuses-canvas%
  (class canvas%
    (override on-event)
    (define on-event (lambda (event) (if (send event button-up? 'left)                                                
                                         (operate-card 'bread-and-circuses bread-and-circuses-canvas bread-and-circuses-gui blue-civ))))
    (super-instantiate ())))

(define (draw-bread-and-circuses canvas dc)
  (send dc clear)
  (update-caller 'bread-and-circuses blue-civ)
  (click-marker canvas))

(define bread-and-circuses-canvas
  (instantiate bread-and-circuses-canvas% ()
    (parent arena-pane)
    (style '(deleted))
    (paint-callback draw-bread-and-circuses)
    (min-height canvas-height)
    (min-width canvas-width)
    (stretchable-width #f) 
    (stretchable-height #f)))

;drama

(define drama-canvas%
  (class canvas%
    (override on-event)
    (define on-event (lambda (event) (if (send event button-up? 'left)                                                
                                         (operate-card 'drama drama-canvas drama-gui blue-civ))))
    (super-instantiate ())))

(define (draw-drama canvas dc)
  (send dc clear)
  (update-caller 'drama blue-civ)
  (click-marker canvas))


(define drama-canvas
  (instantiate drama-canvas% ()
    (parent theater-pane)
    (style '(deleted))
    (paint-callback draw-drama)
    (min-height canvas-height)
    (min-width canvas-width)
    (stretchable-width #f) 
    (stretchable-height #f)))

;theology

(define theology-canvas%
  (class canvas%
    (override on-event)
    (define on-event (lambda (event) (if (send event button-up? 'left)                                                
                                         (operate-card 'theology theology-canvas theology-gui blue-civ))))
    (super-instantiate ())))

(define (draw-theology canvas dc)
  (send dc clear)
  (update-caller 'theology blue-civ)
  (click-marker canvas))

(define theology-canvas
  (instantiate theology-canvas% ()
    (parent temple-pane)
    (paint-callback draw-theology)
    (style '(deleted))
    (min-height canvas-height)
    (min-width canvas-width)
    (stretchable-width #f) 
    (stretchable-height #f)))

;despotism

(define government-canvas%
  (class canvas%
    (super-instantiate ())))

(define (draw-despotism canvas dc)
  (send dc clear)
  (government-update blue-civ)
  (click-marker canvas))

(define government-canvas
  (instantiate government-canvas% ()
    (parent sideways-panel)
    (paint-callback draw-despotism)
    (min-height canvas-height)
    (min-width canvas-width)
    
    (stretchable-width #f)
    (stretchable-height #f)))

;Handutritning

(define blue-hand-frame (instantiate frame% ("Blue Hand")))

(define civil-card-pane (instantiate horizontal-pane% (blue-hand-frame)))

;Hand canvas 1
(define hand-canvas-1%
  (class canvas%
    (override on-event)
    (define on-event (lambda (event) (if (and (send event button-up? 'left)
                                              (equal? (ask main 'current-civ) blue-civ))
                                         (ask main 'play-card 1))))
    (super-instantiate ())))

(define (draw-hand-canvas-1 canvas dc)
  (draw-card (car (car (ask blue-civ 'civil-card-hand))) canvas 0 0))

(define hand-canvas-1
  (instantiate hand-canvas-1% ()
    (parent civil-card-pane)
    (style '(deleted))
    (paint-callback draw-hand-canvas-1)
    (min-height 136)
    (min-width 90)
    (stretchable-width #f) 
    (stretchable-height #f)))

;Hand canvas 2

(define hand-canvas-2%
  (class canvas%
    (override on-event)
    (define on-event (lambda (event) (if (and (send event button-up? 'left)
                                              (equal? (ask main 'current-civ) blue-civ))                                 
                                         (ask main 'play-card 2))))
    (super-instantiate ())))

(define (draw-hand-canvas-2 canvas dc)
  (draw-card (car (list-ref (ask blue-civ 'civil-card-hand) 1)) canvas 0 0))

(define hand-canvas-2
  (instantiate hand-canvas-2% ()
    (parent civil-card-pane)
    (style '(deleted))
    (paint-callback draw-hand-canvas-2)
    (min-height 136)
    (min-width 90)
    (stretchable-width #f) 
    (stretchable-height #f)))

;Hand canvas 3

(define hand-canvas-3%
  (class canvas%
    (override on-event)
    (define on-event (lambda (event) (if (and (send event button-up? 'left)
                                              (equal? (ask main 'current-civ) blue-civ))                                     
                                         (ask main 'play-card 3))))
    (super-instantiate ())))

(define (draw-hand-canvas-3 canvas dc)
  (draw-card (car (list-ref (ask blue-civ 'civil-card-hand) 2)) canvas 0 0))

(define hand-canvas-3
  (instantiate hand-canvas-3% ()
    (parent civil-card-pane)
    (style '(deleted))
    (paint-callback draw-hand-canvas-3)
    (min-height 136)
    (min-width 90)
    (stretchable-width #f) 
    (stretchable-height #f)))

;Hand canvas 4

(define hand-canvas-4%
  (class canvas%
    (override on-event)
    (define on-event (lambda (event) (if (and (send event button-up? 'left)
                                              (equal? (ask main 'current-civ) blue-civ))                                          
                                         (ask main 'play-card 4))))
    (super-instantiate ())))

(define (draw-hand-canvas-4 canvas dc)
  (draw-card (car (list-ref (ask blue-civ 'civil-card-hand) 3)) canvas 0 0))

(define hand-canvas-4
  (instantiate hand-canvas-4% ()
    (parent civil-card-pane)
    (style '(deleted))
    (paint-callback draw-hand-canvas-4)
    (min-height 136)
    (min-width 90)
    (stretchable-width #f) 
    (stretchable-height #f)))

;Hand canvas 5

(define hand-canvas-5%
  (class canvas%
    (override on-event)
    (define on-event (lambda (event) (if (and (send event button-up? 'left)
                                              (equal? (ask main 'current-civ) blue-civ))                                          
                                         (ask main 'play-card 5))))
    (super-instantiate ())))

(define (draw-hand-canvas-5 canvas dc)
  (draw-card (car (list-ref (ask blue-civ 'civil-card-hand) 4)) canvas 0 0))

(define hand-canvas-5
  (instantiate hand-canvas-5% ()
    (parent civil-card-pane)
    (style '(deleted))
    (paint-callback draw-hand-canvas-5)
    (min-height 136)
    (min-width 90)
    (stretchable-width #f) 
    (stretchable-height #f)))
