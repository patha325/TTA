;bitmaps.scm
;Innehåller samtliga bitmapobjekt som används i programmet.

;Skapar ett bitmapobjekt från en fil
(define (make-bitmap filename)
  (make-object bitmap% filename 'png #f))

;Spelare
(define blue-civilization (make-bitmap "blue-player.png"))
(define red-civilization (make-bitmap "Red Player.png"))

;Byggnader
(define bronze-gui (make-bitmap "bronze.png"))
(define iron-gui (make-bitmap "iron.png"))
(define agriculture-gui (make-bitmap "agriculture.png"))
(define irrigation-gui (make-bitmap "irrigation.png"))
(define philosophy-gui (make-bitmap "philosophy.png"))
(define religion-gui (make-bitmap "religion.png"))
(define printing-press-gui (make-bitmap "printing-press.png"))
(define alchemy-gui (make-bitmap "alchemy.png"))
(define bread-and-circuses-gui (make-bitmap "bread-and-circuses.png"))
(define drama-gui (make-bitmap "drama.png"))
(define theology-gui (make-bitmap "theology.png"))

;Teknologier
(define despotism-gui (make-bitmap "despotism.png"))
(define monarchy-gui (make-bitmap "monarchy.png"))
(define theocracy-gui (make-bitmap "theocracy.png"))

;Specialkort
(define mineral-deposits-gui (make-bitmap "mineral deposits.png"))
(define bountiful-harvests-gui (make-bitmap "bountiful harvests.png"))
(define frugality-gui (make-bitmap "frugality.png"))
(define rich-land-gui (make-bitmap "rich land.png"))
(define ideal-building-site-gui (make-bitmap "ideal building site.png"))
(define work-of-art-gui (make-bitmap "work of art.png"))
(define revolutionary-idea-gui (make-bitmap "revolutionary idea.png"))