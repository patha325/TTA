;Red-civ.scm
;Red-civ innehåller allt grafiskt gällande den röda civilizationen.

;Strukturer.

(define red-canvas-frame (instantiate frame% ("Red Civilization")))
(define red-card-panel (instantiate horizontal-panel% (red-canvas-frame)))
(define red-theater-pane (instantiate vertical-pane% (red-card-panel)))
(define red-arena-pane (instantiate vertical-pane% (red-card-panel)))
(define red-library-pane (instantiate vertical-pane% (red-card-panel)))
(define red-lab-pane (instantiate vertical-pane% (red-card-panel)))
(define red-temple-pane (instantiate vertical-pane% (red-card-panel)))
(define red-farm-pane (instantiate vertical-pane% (red-card-panel)))
(define red-mine-pane (instantiate vertical-pane% (red-card-panel)))
(define red-canvas-pane (instantiate vertical-pane% (red-canvas-frame)))
(define red-sideways-panel (instantiate horizontal-panel% (red-canvas-frame)))

(instantiate button% ("End Turn" red-canvas-pane (lambda (button event)
                                                   (if (eq? (ask main 'current-civ) red-civ)
                                                       (ask main 'end-turn)))))

;Utritning.

(define red-civ-canvas%
  (class canvas%
    (override on-event)
    (define on-event (lambda (event) (begin 
                                       (if (and (send event button-up? 'left)
                                                (eq? red-civ (ask main 'current-civ))
                                                (> (send event get-x) 450)
                                                (< (send event get-x) 515)
                                                (> (send event get-y) 20)
                                                (< (send event get-y) 85))
                                           (begin (ask main 'handle 'worker-pool)
                                                  (update-civilization red-civ)
                                                  (click-marker red-civ-canvas)))
                                       (if (and (send event button-up? 'left)
                                                (eq? red-civ (ask main 'current-civ))
                                                (> (send event get-x) 5)
                                                (< (send event get-x) 520)
                                                (> (send event get-y) 90)
                                                (< (send event get-y) 180))
                                           (begin (ask main 'handle 'population-pool)
                                                  (update-civilization red-civ)
                                                  (click-marker red-civ-canvas))))))
    
    (super-instantiate ())))

(define (draw-civilization canvas dc)
  (send dc clear)
  (send dc set-background (make-object color% 255 255 255))
  (send dc draw-bitmap red-civilization 0 0)
  (update-civilization red-civ))

(define red-civ-canvas
  (instantiate red-civ-canvas% ()
    (parent red-sideways-panel)
    (paint-callback draw-civilization)
    (min-height 190)
    (min-width 530)
    (stretchable-width #f) 
    (stretchable-height #f)))

;Kortdefiniering och utritning.

(define canvas-height 134)
(define canvas-width 89)

;bronze

(define red-bronze-canvas%
  (class canvas%
    (override on-event)
    (define on-event (lambda (event) (if (send event button-up? 'left)                                                
                                         (operate-card 'bronze red-bronze-canvas bronze-gui red-civ))))
    (super-instantiate ())))

(define (draw-bronze canvas dc)
  (send dc clear)
  (update-caller 'bronze red-civ)
  (click-marker canvas))

(define red-bronze-canvas
  (instantiate red-bronze-canvas% ()
    (parent red-mine-pane)
    (paint-callback draw-bronze)
    (min-height canvas-height)
    (min-width canvas-width)
    (stretchable-width #f) 
    (stretchable-height #f)))

;iron

(define red-iron-canvas%
  (class canvas%
    (override on-event)
    (define on-event (lambda (event) (if (send event button-up? 'left)                                                
                                         (operate-card 'iron red-iron-canvas iron-gui red-civ))))
    (super-instantiate ())))

(define (draw-iron canvas dc)
  (send dc clear)
  (update-caller 'iron red-civ)
  (click-marker canvas))

(define red-iron-canvas
  (instantiate red-iron-canvas% ()
    (parent red-mine-pane)
    (style '(deleted))
    (paint-callback draw-iron)
    (min-height canvas-height)
    (min-width canvas-width)
    (stretchable-width #f) 
    (stretchable-height #f)))

;agriculture

(define red-agriculture-canvas%
  (class canvas%
    (override on-event)
    (define on-event (lambda (event) (if (send event button-up? 'left)                                                
                                         (operate-card 'agriculture red-agriculture-canvas agriculture-gui red-civ))))
    (super-instantiate ())))

(define (draw-agriculture canvas dc)
  (send dc clear)
  (update-caller 'agriculture red-civ)
  (click-marker canvas))

(define red-agriculture-canvas
  (instantiate red-agriculture-canvas% ()
    (parent red-farm-pane)
    (paint-callback draw-agriculture)
    (min-height canvas-height)
    (min-width canvas-width)
    (stretchable-width #f) 
    (stretchable-height #f)))

;irrigation


(define red-irrigation-canvas%
  (class canvas%
    (override on-event)
    (define on-event (lambda (event) (if (send event button-up? 'left)                                                
                                         (operate-card 'irrigation red-irrigation-canvas irrigation-gui red-civ))))
    (super-instantiate ())))

(define (draw-irrigation canvas dc)
  (send dc clear)
  (update-caller 'irrigation red-civ)
  (click-marker canvas))

(define red-irrigation-canvas
  (instantiate red-irrigation-canvas% ()
    (parent red-farm-pane)
    (style '(deleted))
    (paint-callback draw-irrigation)
    (min-height canvas-height)
    (min-width canvas-width)
    (stretchable-width #f) 
    (stretchable-height #f)))

;philosophy

(define red-philosophy-canvas%
  (class canvas%
    (override on-event)
    (define on-event (lambda (event) (if (send event button-up? 'left)                                                
                                         (operate-card 'philosophy red-philosophy-canvas philosophy-gui red-civ))))
    (super-instantiate ())))

(define (draw-philosophy canvas dc)
  (send dc clear)
  (update-caller 'philosophy red-civ)
  (click-marker canvas))

(define red-philosophy-canvas
  (instantiate red-philosophy-canvas% ()
    (parent red-lab-pane)
    (paint-callback draw-philosophy)
    (min-height canvas-height)
    (min-width canvas-width)
    (stretchable-width #f) 
    (stretchable-height #f)))

;religion

(define red-religion-canvas%
  (class canvas%
    (override on-event)
    (define on-event (lambda (event) (if (send event button-up? 'left)                                                
                                         (operate-card 'religion red-religion-canvas religion-gui red-civ))))
    (super-instantiate ())))

(define (draw-religion canvas dc)
  (send dc clear)
  (update-caller 'religion red-civ)
  (click-marker canvas))

(define red-religion-canvas
  (instantiate red-religion-canvas% ()
    (parent red-temple-pane)
    (paint-callback draw-religion)
    (min-height canvas-height)
    (min-width canvas-width)
    (stretchable-width #f) 
    (stretchable-height #f)))

;printing press

(define red-printing-press-canvas%
  (class canvas%
    (override on-event)
    (define on-event (lambda (event) (if (send event button-up? 'left)                                                
                                         (operate-card 'printing-press red-printing-press-canvas printing-press-gui red-civ))))
    (super-instantiate ())))

(define (draw-printing-press canvas dc)
  (send dc clear)
  (update-caller 'printing-press red-civ)
  (click-marker canvas))

(define red-printing-press-canvas
  (instantiate red-printing-press-canvas% ()
    (parent red-library-pane)
    (style '(deleted))
    (paint-callback draw-printing-press)
    (min-height canvas-height)
    (min-width canvas-width)
    (stretchable-width #f) 
    (stretchable-height #f)))

;alchemy

(define red-alchemy-canvas%
  (class canvas%
    (override on-event)
    (define on-event (lambda (event) (if (send event button-up? 'left)                                                
                                         (operate-card 'alchemy red-alchemy-canvas alchemy-gui red-civ))))
    (super-instantiate ())))

(define (draw-alchemy canvas dc)
  (send dc clear)
  (update-caller 'alchemy red-civ)
  (click-marker canvas))

(define red-alchemy-canvas
  (instantiate red-alchemy-canvas% ()
    (parent red-lab-pane)
    (style '(deleted))
    (paint-callback draw-alchemy)
    (min-height canvas-height)
    (min-width canvas-width)
    (stretchable-width #f) 
    (stretchable-height #f)))

;bread-and-circuses

(define red-bread-and-circuses-canvas%
  (class canvas%
    (override on-event)
    (define on-event (lambda (event) (if (send event button-up? 'left)                                                
                                         (operate-card 'bread-and-circuses red-bread-and-circuses-canvas bread-and-circuses-gui red-civ))))
    (super-instantiate ())))

(define (draw-bread-and-circuses canvas dc)
  (send dc clear)
  (update-caller 'bread-and-circuses red-civ)
  (click-marker canvas))

(define red-bread-and-circuses-canvas
  (instantiate red-bread-and-circuses-canvas% ()
    (parent red-arena-pane)
    (style '(deleted))
    (paint-callback draw-bread-and-circuses)
    (min-height canvas-height)
    (min-width canvas-width)
    (stretchable-width #f) 
    (stretchable-height #f)))

;drama

(define red-drama-canvas%
  (class canvas%
    (override on-event)
    (define on-event (lambda (event) (if (send event button-up? 'left)                                                
                                         (operate-card 'drama red-drama-canvas drama-gui red-civ))))
    (super-instantiate ())))

(define (draw-drama canvas dc)
  (send dc clear)
  (update-caller 'drama red-civ)
  (click-marker canvas))

(define red-drama-canvas
  (instantiate red-drama-canvas% ()
    (parent red-theater-pane)
    (style '(deleted))
    (paint-callback draw-drama)
    (min-height canvas-height)
    (min-width canvas-width)
    (stretchable-width #f) 
    (stretchable-height #f)))

;theology

(define red-theology-canvas%
  (class canvas%
    (override on-event)
    (define on-event (lambda (event) (if (send event button-up? 'left)                                                
                                         (operate-card 'theology red-theology-canvas theology-gui red-civ))))
    (super-instantiate ())))

(define (draw-theology canvas dc)
  (send dc clear)
  (update-caller 'theology red-civ)
  (click-marker canvas))

(define red-theology-canvas
  (instantiate red-theology-canvas% ()
    (parent red-temple-pane)
    (paint-callback draw-theology)
    (style '(deleted))
    (min-height canvas-height)
    (min-width canvas-width)
    (stretchable-width #f) 
    (stretchable-height #f)))

;despotism

(define red-government-canvas%
  (class canvas%
    (super-instantiate ())))

(define (draw-despotism canvas dc)
  
  (send dc clear)
  (government-update red-civ)
  (click-marker canvas))

(define red-government-canvas
  (instantiate red-government-canvas% ()
    (parent red-sideways-panel)
    (paint-callback draw-despotism)
    (min-height canvas-height)
    (min-width canvas-width)
    
    (stretchable-width #f)
    (stretchable-height #f)))

;Handutritning

(define red-hand-frame (instantiate frame% ("Red Hand")))

(define red-civil-card-pane (instantiate horizontal-pane% (red-hand-frame)))


;Hand canvas 1
(define hand-canvas-1%
  (class canvas%
    (override on-event)
    (define on-event (lambda (event) (if (and (send event button-up? 'left)
                                              (equal? (ask main 'current-civ) red-civ))                                     
                                         (ask main 'play-card 1))))
    (super-instantiate ())))

(define (draw-hand-canvas-1 canvas dc)
  (draw-card (car (car (ask red-civ 'civil-card-hand))) canvas 0 0))

(define red-hand-canvas-1
  (instantiate hand-canvas-1% ()
    (parent red-civil-card-pane)
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
                                              (equal? (ask main 'current-civ) red-civ))                                         
                       (ask main 'play-card 2))))
  (super-instantiate ())))

(define (draw-hand-canvas-2 canvas dc)
  (draw-card (car (list-ref (ask red-civ 'civil-card-hand) 1)) canvas 0 0))

(define red-hand-canvas-2
  (instantiate hand-canvas-2% ()
    (parent red-civil-card-pane)
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
                                              (equal? (ask main 'current-civ) red-civ))                                         
                       (ask main 'play-card 3))))
  (super-instantiate ())))

(define (draw-hand-canvas-3 canvas dc)
  (draw-card (car (list-ref (ask red-civ 'civil-card-hand) 2)) canvas 0 0))

(define red-hand-canvas-3
  (instantiate hand-canvas-3% ()
    (parent red-civil-card-pane)
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
                                              (equal? (ask main 'current-civ) red-civ))                                           
                       (ask main 'play-card 4))))
  (super-instantiate ())))

(define (draw-hand-canvas-4 canvas dc)
  (draw-card (car (list-ref (ask red-civ 'civil-card-hand) 3)) canvas 0 0))

(define red-hand-canvas-4
  (instantiate hand-canvas-4% ()
    (parent red-civil-card-pane)
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
                                              (equal? (ask main 'current-civ) red-civ))                                           
                       (ask main 'play-card 5))))
  (super-instantiate ())))

(define (draw-hand-canvas-5 canvas dc)
  (draw-card (car (list-ref (ask red-civ 'civil-card-hand) 4)) canvas 0 0))

(define red-hand-canvas-5
  (instantiate hand-canvas-5% ()
    (parent red-civil-card-pane)
    (style '(deleted))
    (paint-callback draw-hand-canvas-5)
    (min-height 136)
    (min-width 90)
    (stretchable-width #f) 
    (stretchable-height #f)))
