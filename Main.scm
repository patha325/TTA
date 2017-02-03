;Main.scm
;Initierar den speltekniska delen av spelet. Skapar kort, civilisationer, kortlekar och ger korten till brädet.

(load "table-helper.scm")
(load "object-system.scm")
(load "create-civilisation.scm")
(load "create-building.scm")
(load "create-technology.scm")
(load "special-cards.scm")
(load "board.scm")

;Skapar alla byggnader
(define card-mine-bronze-blue (create-card 'mine 'bronze 0 0 2 1 0 0 0))
(define card-mine-bronze-red (create-card 'mine 'bronze 0 0 2 1 0 0 0))
(define card-mine-iron-1 (create-card 'mine 'iron 1 5 5 2 0 0 0))
(define card-mine-iron-2 (create-card 'mine 'iron 1 5 5 2 0 0 0))
(define card-farm-agriculture-blue (create-card 'farm 'agriculture 0 0 2 1 0 0 0))
(define card-farm-agriculture-red (create-card 'farm 'agriculture 0 0 2 1 0 0 0))
(define card-farm-irrigation-1 (create-card 'farm 'irrigation 1 3 4 2 0 0 0))
(define card-farm-irrigation-2 (create-card 'farm 'irrigation 1 3 4 2 0 0 0))
(define card-lab-philosophy-blue (create-card 'lab 'philosophy 0 0 3 0 0 0 1))
(define card-lab-philosophy-red (create-card 'lab 'philosophy 0 0 3 0 0 0 1))
(define card-lab-alchemy-1 (create-card 'lab 'alchemy 1 2 6 0 0 0 2))
(define card-lab-alchemy-2 (create-card 'lab 'alchemy 1 2 6 0 0 0 2))
(define card-temple-religion-blue (create-card 'temple 'religion 0 0 3 0 1 1 0))
(define card-temple-religion-red (create-card 'temple 'religion 0 0 3 0 1 1 0))
(define card-temple-theology (create-card 'temple 'theology 1 2 5 0 2 1 0))
(define card-arena-bread-and-circuses (create-card 'arena 'bread-and-circuses 1 3 4 0 2 0 0))
(define card-theater-drama-1 (create-card 'theater 'drama 1 4 5 0 1 2 0))
(define card-theater-drama-2 (create-card 'theater 'drama 1 4 5 0 1 2 0))
(define card-library-printing-press-1 (create-card 'library 'printing-press 1 3 4 0 0 1 1))
(define card-library-printing-press-2 (create-card 'library 'printing-press 1 3 4 0 0 1 1))

;Skapar alla specialkort
(define card-special-mineral-deposits-1 (create-special-card 'mineral-deposits 1))
(define card-special-mineral-deposits-2 (create-special-card 'mineral-deposits 1))
(define card-special-bountiful-harvests (create-special-card 'bountiful-harvests 1))
(define card-special-frugality (create-special-card 'frugality 1))
(define card-special-rich-land (create-special-card 'rich-land 1))
(define card-special-ideal-building-site (create-special-card 'ideal-building-site 1))
(define card-special-work-of-art (create-special-card 'work-of-art 1))
(define card-special-revolutionary-idea (create-special-card 'revolutionary-idea 1))

;Skapar samtliga övriga teknologier
(define card-government-despotism-blue (create-technology 0 'government 'despotism 0 0 4 2 2 0 0 0 0))
(define card-government-despotism-red (create-technology 0 'government 'despotism 0 0 4 2 2 0 0 0 0))
(define card-government-monarchy (create-technology 1 'government 'monarchy 9 3 5 3 3 0 0 0 0))
(define card-government-theocracy (create-technology 1 'government 'theocracy 7 2 4 3 3 2 1 0 0))


;Ger civilisationer deras grundläggande byggnader, teknologier och arbetare
;Blå civilisation
(define blue-civ (create-civilisation 'blue))

(ask blue-civ 'adjust 'civil-actions 3)
(ask blue-civ 'add-technology card-government-despotism-blue)
(ask blue-civ 'add-technology card-mine-bronze-blue)       
(ask blue-civ 'add-technology card-farm-agriculture-blue)
(ask blue-civ 'add-technology card-lab-philosophy-blue)
(ask blue-civ 'add-technology card-temple-religion-blue)
(ask blue-civ 'building-command 'bronze 'add-worker)
(ask blue-civ 'building-command 'bronze 'add-worker)
(ask blue-civ 'building-command 'agriculture 'add-worker)
(ask blue-civ 'building-command 'agriculture 'add-worker)
(ask blue-civ 'building-command 'philosophy 'add-worker)
(ask blue-civ 'end-of-turn)

;Röd Civilisation
(define red-civ (create-civilisation 'red))

(ask red-civ 'adjust 'civil-actions 2)
(ask red-civ 'add-technology card-government-despotism-red)
(ask red-civ 'add-technology card-mine-bronze-red)
(ask red-civ 'add-technology card-farm-agriculture-red)
(ask red-civ 'add-technology card-lab-philosophy-red)
(ask red-civ 'add-technology card-temple-religion-red)
(ask red-civ 'building-command 'bronze 'add-worker)
(ask red-civ 'building-command 'bronze 'add-worker)
(ask red-civ 'building-command 'agriculture 'add-worker)
(ask red-civ 'building-command 'agriculture 'add-worker)
(ask red-civ 'building-command 'philosophy 'add-worker)
(ask red-civ 'end-of-turn)

;Skapar spelets kortlek.

(define civil-deck-A (shuffle (create-deck 
                               ;byggnader
                               card-mine-iron-1
                               card-mine-iron-2
                               card-farm-irrigation-1 
                               card-farm-irrigation-2
                               card-lab-alchemy-1
                               card-lab-alchemy-2
                               card-temple-theology
                               card-arena-bread-and-circuses
                               card-theater-drama-1
                               card-theater-drama-2
                               card-library-printing-press-1
                               card-library-printing-press-2

                               ;specialkort
                               card-special-mineral-deposits-1
                               card-special-mineral-deposits-2
                               card-special-bountiful-harvests
                               card-special-frugality
                               card-special-rich-land
                               card-special-ideal-building-site
                               card-special-work-of-art
                               card-special-revolutionary-idea

                               ;teknologier
                               card-government-monarchy
                               card-government-theocracy
                               )))

;Ger brädet en ny blandad kortlek
(ask board 'new-deck 'civil civil-deck-A)
(ask board 'add-cards-to-row)
