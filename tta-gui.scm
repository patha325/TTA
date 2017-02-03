;tta-gui.scm
(load "gui-display.scm")
(load "object-system.scm")

;Tta-gui innehåller alla våra grafiska funktioner.

;Huvudrutan.
(define frame (instantiate frame% ("Throught The Ages")))  

(define panel (instantiate horizontal-panel% (frame)))

(define pane-1 (instantiate vertical-pane% (panel)))
(define pane-2 (instantiate vertical-pane% (panel)))

(instantiate button% ("Blue Civilization" pane-1 (lambda (button event) 
                                                   (begin (frame-hide/show canvas-frame)
                                                          (update-civilization blue-civ)
                                                          ))))
(instantiate button% ("Blue Hand" pane-1 (lambda (button event) 
                                           (frame-hide/show blue-hand-frame)
                                           (draw-hand blue-civ))))
(instantiate button% ("Red Civilization" pane-2 (lambda (button event) 
                                                  (begin (frame-hide/show red-canvas-frame)
                                                         (update-civilization red-civ)
                                                         ))))
(instantiate button% ("Red Hand" pane-2 (lambda (button event) 
                                          (frame-hide/show red-hand-frame)
                                          (draw-hand red-civ))))

(instantiate button% ("Main Board" panel (lambda (button event)
                                           (frame-hide/show board-frame)
                                           )))
(define board-frame (instantiate frame% ("Main Board")))

(define (frame-hide/show frame)
  (if (send frame is-shown?)
      (send frame show #f)
      (send frame show #t)))

(send frame show #t)

;Våra egna penslar och text.
(define my-font (make-object font% 15 'default 'normal 'bold #f 'default #f))
(define marker-brush (make-object brush% (make-object color% 255 255 255) 'transparent))
(define marker-pen (make-object pen% (make-object color% 255 255 0) 4 'solid))

;Ritar ut handen för en civilization.        
(define (draw-hand civ-object)
  (remove-card-hand (send (get-hand-pane civ-object) get-children) civ-object)
  (draw-hand-help (ask civ-object 'civil-card-hand) 1 civ-object))

(define (draw-hand-help card-hand index civ-object)
  (if (not (empty? card-hand))
      (begin (send (get-hand-pane civ-object) add-child (get index 'canvas civ-object)) 
             (draw-card (caar card-hand) (get index 'canvas civ-object) 0 0)
             (draw-hand-help (cdr card-hand) (+ index 1) civ-object))))
(define (get-hand-pane civ-object)
  (cond ((equal? civ-object blue-civ) civil-card-pane)
        ((equal? civ-object red-civ) red-civil-card-pane)))

;Tar bort samtliga kort från korthanden - förberedelse för att rita ut handen.
(define (remove-card-hand child-list civ-object)
  (if (not (empty? child-list))
      (begin (send (get-hand-pane civ-object) delete-child (car child-list))
             (remove-card-hand (cdr child-list) civ-object))))

;Uppdaterar resurser på mines och farms.
(define (update-affected-cards card-type)                                                     
  (cond
    ((eq? card-type 'mine) (update-mines (ask (ask main 'current-civ) 'get-all-lists)))                      
    ((eq? card-type 'farm) (update-farms (ask (ask main 'current-civ) 'get-all-lists)))))

(define (update-mines building-list)
  (cond ((assq 'bronze building-list) (update-caller 'bronze (ask main 'current-civ)))
        ((assq 'iron building-list) (update-caller 'iron (ask main 'current-civ)))))

(define (update-farms building-list)
  (cond ((assq 'agriculture building-list) (update-caller 'agriculture (ask main 'current-civ)))
        ((assq 'irrigation building-list) (update-caller 'irrigation (ask main 'current-civ)))))

;Uppdaterar governmentkortet.
(define (government-update civ-object)           
  (send (send (get (ask civ-object 'current-government) 'canvas civ-object) get-dc) clear)
  (send (send (get (ask civ-object 'current-government) 'canvas civ-object) get-dc) draw-bitmap (get (ask civ-object 'current-government) 'graphic civ-object) 0 0)
  (my-draw-circle (get (ask civ-object 'current-government) 'canvas civ-object) 6 5 21 21 *black-pen* *white-brush*)
  (send (send (get (ask civ-object 'current-government) 'canvas civ-object) get-dc) set-font my-font)
  (send (send (get (ask civ-object 'current-government) 'canvas civ-object) get-dc) draw-text (format "~a" (ask civ-object 'civil-actions)) 10 6)
  (my-draw-circle (get (ask civ-object 'current-government) 'canvas civ-object) 60 5 21 21 *black-pen* *red-brush*)
  (send (send (get (ask civ-object 'current-government) 'canvas civ-object) get-dc) draw-text (format "~a" (ask civ-object 'military-actions)) 67 6))

;Ser till att det man trycker på markeras.
(define (click-marker canvas-name)
  (send (send canvas-name get-dc) set-brush marker-brush)
  (send (send canvas-name get-dc) set-pen marker-pen)
  (cond ((eq? (ask main 'origin-click) 'worker-pool) 
         (send (send (get (ask main 'current-civ) 'canvas (ask main 'current-civ)) get-dc) draw-rectangle 453 22 62 62))
        ((eq? (ask main 'origin-click) 'population-pool)
         (send (send (get (ask main 'current-civ) 'canvas (ask main 'current-civ)) get-dc) draw-rectangle 10 93 510 88))
        ((building? (ask main 'origin-click))
         (send (send canvas-name get-dc) draw-rectangle 5 5 80 128))))

;Grundläggande utritningsfunktioner.
(define (my-draw-circle canvas x y size-x size-y pen brush)
  (send (send canvas get-dc) set-pen pen)
  (send (send canvas get-dc) set-brush brush)
  (send (send canvas get-dc) draw-ellipse x y size-x size-y))

(define (my-draw-rectangle canvas x y size-x size-y pen brush)
  (send (send canvas get-dc) set-pen pen)
  (send (send canvas get-dc) set-brush brush)
  (send (send canvas get-dc) draw-rectangle x y size-x size-y))

;Ritar kortraden på brädet.
(define (draw-card-row)
  (draw-card-row-help (ask board 'card-row) 56 92))
(define (draw-card-row-help card-row x-pos y-pos)
  (cond ((and (not (empty? card-row)) (empty? (car card-row))) (draw-card-row-help (cdr card-row) (+ x-pos 91) y-pos))
        ((not (empty? card-row)) (begin (draw-card (ask (car card-row) 'name) upper-board-canvas x-pos y-pos)
                                        (draw-card-row-help (cdr card-row) (+ x-pos 91) y-pos)))))

;Ritar ut civilisationen.
(define (update-civilization civ-object)
  (send (send (get civ-object 'canvas civ-object) get-dc) clear)
  (send (send (get civ-object 'canvas civ-object) get-dc) draw-bitmap (get civ-object 'graphic civ-object) 0 0)
  (government-update civ-object)
  (draw-tokens (ask civ-object 'tokens) civ-object)
  (draw-population (ask civ-object 'population-pool )civ-object)
  (draw-happiness (ask civ-object 'sum/turn 'happiness) civ-object)
  
  (if (< 0 (ask civ-object 'worker-pool))
      (begin (send (send (get civ-object 'canvas civ-object) get-dc) set-font my-font)
             (my-draw-circle (get civ-object 'canvas civ-object) 485 21 28 28 *black-pen* *yellow-brush*)
             (send (send (get civ-object 'canvas civ-object) get-dc) draw-text (format "~a" (ask civ-object 'worker-pool)) 493 25))))

(define (update-caller building-name civ-object)
  (card-update (get building-name 'canvas civ-object) building-name (get building-name 'graphic civ-object) civ-object))        

;Uppdaterar ett givet kort.
(define (card-update canvas-name card-name picture-name civ-object)
  (send (send canvas-name get-dc) clear)
  (send (send canvas-name get-dc) draw-bitmap picture-name 0 0)
  (send (send canvas-name get-dc) set-font my-font)
  (if (< 0 (ask civ-object 'building-command card-name 'workers))
      (begin (my-draw-circle canvas-name 6 5 21 21 *black-pen* *yellow-brush*)
             (send (send canvas-name get-dc) draw-text (format "~a" (ask civ-object 'building-command card-name 'workers)) 10 6)))
  (if (< 0 (ask civ-object 'building-command card-name 'tokens))
      (begin (my-draw-circle canvas-name 60 5 21 21 *black-pen* *blue-brush*)
             (send (send canvas-name get-dc) draw-text (format "~a" (ask civ-object 'building-command card-name 'tokens)) 67 6)))
  ) 

;Hanterar kortaktiveringar.
(define (operate-card name canvas-name picture-name civ)
  (if (equal? civ (ask main 'current-civ))
      (begin 
        (ask main 'handle name)
        (card-update canvas-name name picture-name civ)
        (click-marker canvas-name)
        )))

;Är det en byggnad?
(define (building? name)
  (if (assq name mighty-list) 
      #t
      #f))

;Aktiverar ett kortcanvas.
(define (activate-canvas card-object)
  (send (get-pane (ask card-object 'type)) add-child (get (ask card-object 'name) 'canvas (ask main 'current-civ))))

;Häntar olika detaljer från mighty-list
(define (get card-name detail civ-object)
  (cond ((and (equal? civ-object blue-civ) (eq? detail 'canvas)) (cadr (get-cell card-name mighty-list)))
        ((and (equal? civ-object red-civ) (eq? detail 'canvas)) (caddr (get-cell card-name mighty-list)))
        ((eq? detail 'graphic) (cadddr (get-cell card-name mighty-list)))
        ))

(define (get-cell card-name mighty-list)
  (cond ((empty? mighty-list) #f)
        ((eq? (car (car mighty-list)) card-name) (car mighty-list))
        (else (get-cell card-name (cdr mighty-list)))))

;Häntar det paneobjekt som tillhör kortet (och civilisationen).
(define (get-pane card-type)
  (cond ((equal? (ask main 'current-civ) blue-civ)
         (cond ((eq? card-type 'mine) mine-pane)
               ((eq? card-type 'farm) farm-pane)
               ((eq? card-type 'temple) temple-pane)
               ((eq? card-type 'lab) lab-pane)
               ((eq? card-type 'library) library-pane)
               ((eq? card-type 'arena) arena-pane)
               ((eq? card-type 'government) sideways-panel)
               ((eq? card-type 'theater) theater-pane)))
        ((equal? (ask main 'current-civ) red-civ)
         (cond ((eq? card-type 'mine) red-mine-pane)
               ((eq? card-type 'farm) red-farm-pane)
               ((eq? card-type 'temple) red-temple-pane)
               ((eq? card-type 'lab) red-lab-pane)
               ((eq? card-type 'library) red-library-pane)
               ((eq? card-type 'arena) red-arena-pane)
               ((eq? card-type 'government) red-sideways-panel)
               ((eq? card-type 'theater) red-theater-pane)))))

;Ritar ut ett kort, på en given position.
(define (draw-card card-name canvas x-pos y-pos)
  (send (send canvas get-dc) draw-bitmap (get card-name 'graphic (ask main 'current-civ)) x-pos y-pos))

;Mighty-list associerar samtliga objekt med varandra.
;name blue-canvas red-canvas picture-name
(define mighty-list
  (list (list 1 hand-canvas-1 red-hand-canvas-1 'nil)
        (list 2 hand-canvas-2 red-hand-canvas-2 'nil)
        (list 3 hand-canvas-3 red-hand-canvas-3 'nil)
        (list 4 hand-canvas-4 red-hand-canvas-4 'nil)
        (list 5 hand-canvas-5 red-hand-canvas-5 'nil)
        ;buildings
        (list 'bronze bronze-canvas red-bronze-canvas bronze-gui)
        (list 'iron iron-canvas red-iron-canvas iron-gui)
        (list 'agriculture agriculture-canvas red-agriculture-canvas agriculture-gui)
        (list 'irrigation irrigation-canvas red-irrigation-canvas irrigation-gui)
        (list 'religion religion-canvas red-religion-canvas religion-gui)
        (list 'theology theology-canvas red-theology-canvas theology-gui)
        (list 'philosophy philosophy-canvas red-philosophy-canvas philosophy-gui)
        (list 'alchemy alchemy-canvas red-alchemy-canvas alchemy-gui)
        (list 'printing-press printing-press-canvas red-printing-press-canvas printing-press-gui)
        (list 'bread-and-circuses bread-and-circuses-canvas red-bread-and-circuses-canvas bread-and-circuses-gui)
        (list 'drama drama-canvas red-drama-canvas drama-gui)
        ;technologies
        (list 'despotism government-canvas red-government-canvas despotism-gui)
        (list 'monarchy government-canvas red-government-canvas monarchy-gui) 
        (list 'theocracy government-canvas red-government-canvas theocracy-gui)
        ;special cards
        (list 'mineral-deposits 'nil 'nil mineral-deposits-gui)
        (list 'bountiful-harvests 'nil 'nil bountiful-harvests-gui)
        (list 'frugality 'nil 'nil frugality-gui)
        (list 'rich-land 'nil 'nil rich-land-gui)
        (list 'ideal-building-site 'nil 'nil ideal-building-site-gui)
        (list 'work-of-art 'nil 'nil work-of-art-gui)
        (list 'revolutionary-idea 'nil 'nil revolutionary-idea-gui)
        
        (list red-civ 'nil red-civ-canvas red-civilization) 
        (list blue-civ blue-civ-canvas 'nil blue-civilization)
        ))

;Ritar ut resursbanken på civilizationen.
(define (draw-tokens token-number civ-object)
  (cond 
    ((= 18 token-number) (draw-token 319 57 civ-object))
    ((= 17 token-number) (draw-token 319 30 civ-object))
    ((= 16 token-number) (draw-token 291 57 civ-object))
    ((= 15 token-number) (draw-token 291 30 civ-object))
    ((= 14 token-number) (draw-token 263 57 civ-object))
    ((= 13 token-number) (draw-token 263 30 civ-object))
    ((= 12 token-number) (draw-token 235 57 civ-object))
    ((= 11 token-number) (draw-token 235 30 civ-object))
    ((= 10 token-number) (draw-token 208 57 civ-object))
    ((= 9 token-number) (draw-token 208 30 civ-object))
    ((= 8 token-number) (draw-token 151 57 civ-object))
    ((= 7 token-number) (draw-token 151 30 civ-object))
    ((= 6 token-number) (draw-token 124 57 civ-object))
    ((= 5 token-number) (draw-token 124 30 civ-object))
    ((= 4 token-number) (draw-token 67 57 civ-object))
    ((= 3 token-number) (draw-token 67 30 civ-object))
    ((= 2 token-number) (draw-token 40 57 civ-object))
    ((= 1 token-number) (draw-token 40 30 civ-object)))
  (if (< 0 token-number)
      (draw-tokens (- token-number 1) civ-object)))

;Ritar ut populationbanken på civilizationen.
(define (draw-population worker-number civ-object)
  (cond 
    ((= 18 worker-number) (draw-worker 380 149 civ-object))
    ((= 17 worker-number) (draw-worker 380 122 civ-object))
    ((= 16 worker-number) (draw-worker 319 149 civ-object))
    ((= 15 worker-number) (draw-worker 319 122 civ-object))
    ((= 14 worker-number) (draw-worker 291 149 civ-object))
    ((= 13 worker-number) (draw-worker 291 122 civ-object))
    ((= 12 worker-number) (draw-worker 235 149 civ-object))
    ((= 11 worker-number) (draw-worker 235 122 civ-object))
    ((= 10 worker-number) (draw-worker 208 149 civ-object))
    ((= 9 worker-number) (draw-worker 208 122 civ-object))
    ((= 8 worker-number) (draw-worker 151 149 civ-object))
    ((= 7 worker-number) (draw-worker 151 122 civ-object))
    ((= 6 worker-number) (draw-worker 124 149 civ-object))
    ((= 5 worker-number) (draw-worker 124 122 civ-object))
    ((= 4 worker-number) (draw-worker 67 149 civ-object))
    ((= 3 worker-number) (draw-worker 67 122 civ-object))
    ((= 2 worker-number) (draw-worker 40 149 civ-object))
    ((= 1 worker-number) (draw-worker 40 122 civ-object)))
  
  (if (< 0 worker-number)
      (draw-population (- worker-number 1) civ-object)))

;Ritar ut happiness på civilizationen.
(define (draw-happiness happiness civ-object)
  (cond 
    ((<= 8 happiness) (draw-my-happiness 38 95 civ-object)) ;8+
    ((= 7 happiness) (draw-my-happiness 73 95 civ-object)) ;7
    ((= 6 happiness) (draw-my-happiness 123 95 civ-object)) ;6
    ((= 5 happiness) (draw-my-happiness 157 95 civ-object)) ;5
    ((= 4 happiness) (draw-my-happiness 208 95 civ-object)) ;4
    ((= 3 happiness) (draw-my-happiness 241 95 civ-object)) ;3
    ((= 2 happiness) (draw-my-happiness 309 95 civ-object)) ;2 
    ((= 1 happiness) (draw-my-happiness 427 95 civ-object)) ;1
    ((= 0 happiness) (draw-my-happiness 497 95 civ-object)))) ;0

;Uppdaterar lägre delen av brädet
(define (update-lower-board)
  (send (send lower-board-canvas get-dc) clear)
  (send (send lower-board-canvas get-dc) draw-bitmap lower-board-gui 3 0)
  (draw-culture/turn)
  (draw-science)
  (draw-science/turn)
  )

;För de undre funktionerna
(define y 4)

;Ritar ut alla civilizationernas kultur på brädet.
(define (draw-culture)
  (my-draw-culture (ask blue-civ 'culture) 'blue upper-board-canvas)
  (my-draw-culture (ask red-civ 'culture) 'red upper-board-canvas))

(define (my-draw-culture culture color canvas)
  (if (eq? color 'blue)
      (set! y 25)
      (set! y 14))
  (cond
    ((= 30 culture) (draw-my-rectangle 555 y color canvas))
    ((= 29 culture) (draw-my-rectangle 537 y color canvas))
    ((= 28 culture) (draw-my-rectangle 519 y color canvas))
    ((= 27 culture) (draw-my-rectangle 502 y color canvas))
    ((= 26 culture) (draw-my-rectangle 489 y color canvas))
    ((= 25 culture) (draw-my-rectangle 470 y color canvas))
    ((= 24 culture) (draw-my-rectangle 451 y color canvas))
    ((= 23 culture) (draw-my-rectangle 435 y color canvas))
    ((= 22 culture) (draw-my-rectangle 420 y color canvas))
    ((= 21 culture) (draw-my-rectangle 403 y color canvas))
    ((= 20 culture) (draw-my-rectangle 387 y color canvas))
    ((= 19 culture) (draw-my-rectangle 370 y color canvas))
    ((= 18 culture) (draw-my-rectangle 351 y color canvas))
    ((= 17 culture) (draw-my-rectangle 333 y color canvas))
    ((= 16 culture) (draw-my-rectangle 313 y color canvas))
    ((= 15 culture) (draw-my-rectangle 298 y color canvas))
    ((= 14 culture) (draw-my-rectangle 286 y color canvas))
    ((= 13 culture) (draw-my-rectangle 266 y color canvas))
    ((= 12 culture) (draw-my-rectangle 246 y color canvas))
    ((= 11 culture) (draw-my-rectangle 226 y color canvas))
    ((= 10 culture) (draw-my-rectangle 211 y color canvas))
    ((= 9 culture) (draw-my-rectangle 198 y color canvas))
    ((= 8 culture) (draw-my-rectangle 178 y color canvas))
    ((= 7 culture) (draw-my-rectangle 158 y color canvas))
    ((= 6 culture) (draw-my-rectangle 138 y color canvas))
    ((= 5 culture) (draw-my-rectangle 123 y color canvas))
    ((= 4 culture) (draw-my-rectangle 108 y color canvas))
    ((= 3 culture) (draw-my-rectangle 88 y color canvas))
    ((= 2 culture) (draw-my-rectangle 68 y color canvas))
    ((= 1 culture) (draw-my-rectangle 48 y color canvas))   
    ((= 0 culture) (draw-my-rectangle 20 y color canvas)))) ;0

;Ritar ut alla civilizationernas science på brädet.
(define (draw-science)
  (my-draw-science (ask blue-civ 'science) 'blue lower-board-canvas)
  (my-draw-science (ask red-civ 'science) 'red lower-board-canvas))

(define (my-draw-science science color canvas)
  (if (eq? color 'blue)
      (set! y 49)
      (set! y 38))
  (cond
    ((= 15 science) (draw-my-rectangle 481 y color canvas)) ;15
    ((= 14 science) (draw-my-rectangle 465 y color canvas)) ;14
    ((= 13 science) (draw-my-rectangle 440 y color canvas)) ;13
    ((= 12 science) (draw-my-rectangle 423 y color canvas)) ;12
    ((= 11 science) (draw-my-rectangle 406 y color canvas)) ;11
    ((= 10 science) (draw-my-rectangle 385 y color canvas)) ;10
    ((= 9 science) (draw-my-rectangle 350 y color canvas)) ;9
    ((= 8 science) (draw-my-rectangle 319 y color canvas)) ;8
    ((= 7 science) (draw-my-rectangle 288 y color canvas)) ;7
    ((= 6 science) (draw-my-rectangle 257 y color canvas)) ;6
    ((= 5 science) (draw-my-rectangle 225 y color canvas)) ;5
    ((= 4 science) (draw-my-rectangle 195 y color canvas)) ;4
    ((= 3 science) (draw-my-rectangle 164 y color canvas)) ;3
    ((= 2 science) (draw-my-rectangle 133 y color canvas)) ;2
    ((= 1 science) (draw-my-rectangle 102 y color canvas)) ;1
    ((= 0 science) (draw-my-rectangle 69 y color canvas)))) ;0

;Ritar ut alla civilizationernas science per turn på brädet.
(define (draw-science/turn)
  (my-draw-science/turn (ask blue-civ 'sum/turn 'science) 'blue lower-board-canvas)
  (my-draw-science/turn (ask red-civ 'sum/turn 'science) 'red lower-board-canvas))

(define (my-draw-science/turn science color canvas)
  (if (eq? color 'blue)
      (set! y 285)
      (set! y 274))
  (cond
    ((= 15 science) (draw-my-rectangle 481 y color canvas)) ;15
    ((= 14 science) (draw-my-rectangle 465 y color canvas)) ;14
    ((= 13 science) (draw-my-rectangle 440 y color canvas)) ;13
    ((= 12 science) (draw-my-rectangle 423 y color canvas)) ;12
    ((= 11 science) (draw-my-rectangle 406 y color canvas)) ;11
    ((= 10 science) (draw-my-rectangle 385 y color canvas)) ;10
    ((= 9 science) (draw-my-rectangle 350 y color canvas)) ;9
    ((= 8 science) (draw-my-rectangle 319 y color canvas)) ;8
    ((= 7 science) (draw-my-rectangle 288 y color canvas)) ;7
    ((= 6 science) (draw-my-rectangle 257 y color canvas)) ;6
    ((= 5 science) (draw-my-rectangle 225 y color canvas)) ;5
    ((= 4 science) (draw-my-rectangle 195 y color canvas)) ;4
    ((= 3 science) (draw-my-rectangle 164 y color canvas)) ;3
    ((= 2 science) (draw-my-rectangle 133 y color canvas)) ;2
    ((= 1 science) (draw-my-rectangle 102 y color canvas)) ;1
    ((= 0 science) (draw-my-rectangle 69 y color canvas)))) ;0

;Ritar ut alla civilizationernas kultur per turn på brädet.
(define (draw-culture/turn)
  (my-draw-culture/turn (ask blue-civ 'sum/turn 'culture) 'blue lower-board-canvas)
  (my-draw-culture/turn (ask red-civ 'sum/turn 'culture) 'red lower-board-canvas))

(define (my-draw-culture/turn culture color canvas)
  (if (eq? color 'blue)
      (set! y 228)
      (set! y 217))
  (cond
    ((= 15 culture) (draw-my-rectangle 481 y color canvas)) ;15
    ((= 14 culture) (draw-my-rectangle 465 y color canvas)) ;14
    ((= 13 culture) (draw-my-rectangle 440 y color canvas)) ;13
    ((= 12 culture) (draw-my-rectangle 423 y color canvas)) ;12
    ((= 11 culture) (draw-my-rectangle 406 y color canvas)) ;11
    ((= 10 culture) (draw-my-rectangle 385 y color canvas)) ;10
    ((= 9 culture) (draw-my-rectangle 350 y color canvas)) ;9
    ((= 8 culture) (draw-my-rectangle 319 y color canvas)) ;8
    ((= 7 culture) (draw-my-rectangle 288 y color canvas)) ;7
    ((= 6 culture) (draw-my-rectangle 257 y color canvas)) ;6
    ((= 5 culture) (draw-my-rectangle 225 y color canvas)) ;5
    ((= 4 culture) (draw-my-rectangle 195 y color canvas)) ;4
    ((= 3 culture) (draw-my-rectangle 164 y color canvas)) ;3
    ((= 2 culture) (draw-my-rectangle 133 y color canvas)) ;2
    ((= 1 culture) (draw-my-rectangle 102 y color canvas)) ;1
    ((= 0 culture) (draw-my-rectangle 69 y color canvas)))) ;0

(define (draw-my-rectangle x y color canvas)
  (cond
    ((eq? 'blue color) (my-draw-rectangle canvas x y 13 13 *black-pen* *blue-brush*))
    ((eq? 'red color) (my-draw-rectangle canvas x y 13 13 *black-pen* *red-brush*))))

(define (draw-my-happiness x y civ-object)
  (cond
    ((eq? blue-civ civ-object) (my-draw-rectangle blue-civ-canvas x y 21 20 *black-pen* *blue-brush*))
    ((eq? red-civ civ-object) (my-draw-rectangle red-civ-canvas x y 21 20 *black-pen* *red-brush*)))) 

(define (draw-worker x y civ-object)
  (cond
    ((eq? blue-civ civ-object) (my-draw-circle blue-civ-canvas x y 21 20 *black-pen* *yellow-brush*))
    ((eq? red-civ civ-object) (my-draw-circle red-civ-canvas x y 21 20 *black-pen* *yellow-brush*)))) 

(define (draw-token x y civ-object)
  (cond
    ((eq? blue-civ civ-object) (my-draw-circle blue-civ-canvas x y 21 20 *black-pen* *blue-brush*))
    ((eq? red-civ civ-object) (my-draw-circle red-civ-canvas x y 21 20 *black-pen* *blue-brush*)))) 

;Brädet

(define main-board-pane (instantiate vertical-pane% (board-frame)))

(define upper-board-gui (make-object bitmap% "main-board-upper.png" 'png #f))
(define lower-board-gui (make-object bitmap% "main-board-bottom.png" 'png #f))

;Övre brädet.

(define (update-upper-board)
  (send (send upper-board-canvas get-dc) clear)
  (send (send upper-board-canvas get-dc) draw-bitmap upper-board-gui 0 0)
  (draw-card-row)
  (draw-culture)
  )

(define card-width 80)

(define upper-board-canvas%
  (class canvas%
    (override on-event)
    (define on-event (lambda (event) (begin
                                       ;First card
                                       (if (and (send event button-up? 'left)
                                                (> (send event get-x) 60)
                                                (< (send event get-x) 150)
                                                (> (send event get-y) 90)
                                                (< (send event get-y) 215))
                                           (ask main 'handle-card 1))
                                       
                                       ;Second card
                                       (if (and (send event button-up? 'left)
                                                (> (send event get-x) (+ 60 card-width))
                                                (< (send event get-x) 240)
                                                (> (send event get-y) 90)
                                                (< (send event get-y) 215))
                                           (ask main 'handle-card 2))
                                       ;Third card
                                       (if (and (send event button-up? 'left)
                                                (> (send event get-x) 240)
                                                (< (send event get-x) 330)
                                                (> (send event get-y) 90)
                                                (< (send event get-y) 215))
                                           (ask main 'handle-card 3))
                                       
                                       ;Fourth card
                                       (if (and (send event button-up? 'left)
                                                (> (send event get-x) 330)
                                                (< (send event get-x) 420)
                                                (> (send event get-y) 90)
                                                (< (send event get-y) 215)) 
                                           (ask main 'handle-card 4))
                                       
                                       ;Fifth card
                                       (if (and (send event button-up? 'left)
                                                (> (send event get-x) 420)
                                                (< (send event get-x) 510)
                                                (> (send event get-y) 90)
                                                (< (send event get-y) 215))
                                           (ask main 'handle-card 5))
                                       ;Sixth card
                                       (if (and (send event button-up? 'left)
                                                (> (send event get-x) 510)
                                                (< (send event get-x) 600)
                                                (> (send event get-y) 90)
                                                (< (send event get-y) 215))
                                           (ask main 'handle-card 6))
                                       ;Seventh card
                                       (if (and (send event button-up? 'left)
                                                (> (send event get-x) 600)
                                                (< (send event get-x) 690)
                                                (> (send event get-y) 90)
                                                (< (send event get-y) 215))
                                           (ask main 'handle-card 7))
                                       ;Eight card
                                       (if (and (send event button-up? 'left)
                                                (> (send event get-x) 690)
                                                (< (send event get-x) 780)
                                                (> (send event get-y) 90)
                                                (< (send event get-y) 215))
                                           (ask main 'handle-card 8))
                                       ;Ninth card
                                       (if (and (send event button-up? 'left)
                                                (> (send event get-x) 780)
                                                (< (send event get-x) 870)
                                                (> (send event get-y) 90)
                                                (< (send event get-y) 215))
                                           (ask main 'handle-card 9))
                                       ;Tenth card
                                       (if (and (send event button-up? 'left)
                                                (> (send event get-x) 870)
                                                (< (send event get-x) 960)
                                                (> (send event get-y) 90)
                                                (< (send event get-y) 215))
                                           (ask main 'handle-card 10))
                                       ;Eleventh card
                                       (if (and (send event button-up? 'left)
                                                (> (send event get-x) 960)
                                                (< (send event get-x) 1050)
                                                (> (send event get-y) 90)
                                                (< (send event get-y) 215))
                                           (ask main 'handle-card 11))
                                       ;Twelfth card
                                       (if (and (send event button-up? 'left)
                                                (> (send event get-x) 1050)
                                                (< (send event get-x) 1140)
                                                (> (send event get-y) 90)
                                                (< (send event get-y) 215))
                                           (ask main 'handle-card 12))
                                       ;Thirtenth card
                                       (if (and (send event button-up? 'left)
                                                (> (send event get-x) 1140)
                                                (< (send event get-x) 1230)
                                                (> (send event get-y) 90)
                                                (< (send event get-y) 215))
                                           (ask main 'handle-card 13)))))                                                              
    (super-instantiate ())))

(define (draw-upper-board canvas dc)
  (send dc clear)
  (update-upper-board))

(define upper-board-canvas
  (instantiate upper-board-canvas% ()
    (parent main-board-pane)
    (paint-callback draw-upper-board)
    (min-height 250)
    (min-width 1300)
    
    (stretchable-width #f)
    (stretchable-height #f)))

;Undre brädet.
(define lower-board-canvas%
  (class canvas%
    (override on-event)
    (define on-event (lambda (event) (begin
                                       (if (and (send event button-up? 'left)
                                                (> (send event get-x) 450)
                                                (< (send event get-x) 515)
                                                (> (send event get-y) 20)
                                                (< (send event get-y) 85))
                                           (begin (
                                                   )))
                                       (if (and (send event button-up? 'left)
                                                (> (send event get-x) 5)
                                                (< (send event get-x) 520)
                                                (> (send event get-y) 90)
                                                (< (send event get-y) 180))
                                           (begin (
                                                   ))))))
    
    (super-instantiate ())))

(define (draw-lower-board canvas dc)
  (send dc clear)
  (update-lower-board))

(define lower-board-canvas
  (instantiate lower-board-canvas% ()
    (parent main-board-pane)
    (paint-callback draw-lower-board)
    (min-height 370)
    (min-width 1300)
    
    (stretchable-width #f)
    (stretchable-height #f)))

;Revolution window
(define revolution-frame (instantiate dialog% ("Revolution")))
(define revolution-panel (instantiate horizontal-panel% (revolution-frame)))

(instantiate button% ("Peaceful Revolution" revolution-panel (lambda (button event)
                                                               (begin (send revolution-frame show #f)
                                                                      (ask main 'peaceful-revolution)
                                                                      ))))

(instantiate button% ("Military Revolution" revolution-panel (lambda (button event)
                                                               (begin (send revolution-frame show #f)
                                                                      (ask main 'military-revolution)))))

;Text-log, ritar ut kommentarer i text-loggan som spelaren kan få upp.
(instantiate button% ("Text Log" panel (lambda (button event)
                                         (begin (frame-hide/show text-frame)
                                                (send (send text-log get-editor) lock #t)
                                                ))))

(define text-frame (instantiate frame% ("Text Log")))

(define (void-fn text-field text-event)
  #f)

(define text-log 
  (new text-field%
       (label #f)
       (parent text-frame)
       (callback void-fn)
       (init-value "Welcome to Through the Ages! Red Civilization starts.")
       (style '(multiple hscroll))
       (min-width 500)    
       (min-height 150)
       (stretchable-width #t)
       (stretchable-height #t)))

(define (write-log . args)
  
  (send (send text-log get-editor) lock #f)
  (send (send text-log get-editor) insert #\newline)
  (cond
    ((eq? (ask main 'current-civ) blue-civ) (send (send text-log get-editor) insert "Blue civilization "))      
    ((eq? (ask main 'current-civ) red-civ) (send (send text-log get-editor) insert "Red civilization ")))
  (write-arguments args)
  (send (send text-log get-editor) lock #t))

(define (write-arguments args)
  (if (not (empty? args))
      (begin (send (send text-log get-editor) insert (car args))
             (write-arguments (cdr args)))))