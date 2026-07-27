#lang racket/gui
(require racket/gui/base)
(require "constanten.rkt")

(provide gui% vraag-gebruiker-modus)

(struct trein
  (paneel
   id-keuze
   vorig-keuze
   huidig-keuze
   actieve-lijst
   snelheid-slider
   bestemming-keuze
   noodstop)
  #:transparent)

(struct wissel
  (paneel
   keuze-lijst 
   stand-radio)
  #:transparent)

(struct slagboom
  (paneel 
   keuze-lijst 
   positie-radio)
  #:transparent)

(struct verkeerslicht
  (paneel 
   keuze-lijst 
   licht-radio)
  #:transparent)

(struct overzicht
  (paneel
   msg)
  #:transparent)

;; MODUS KIEZEN
(define (vraag-gebruiker-modus)
  (define gekozen-modus #f) 
  
  (define dialoog (new dialog% 
                       [label "Voorpagina modus"]
                       [width DIALOOG_BREEDTE]
                       [height DIALOOG_HOOGTE]))
  
  (define vert-panel (new vertical-panel% [parent dialoog] [alignment '(center center)] [spacing SPACING]))

  (new message% [parent vert-panel] 
       [label "Selecteer jouw modus."])
  
  (define knoppen-paneel (new horizontal-panel% [parent vert-panel] 
                              [alignment '(center center)]
                              [spacing SPACING]))

  (define (maak-knop label modus)
    (new button% [parent knoppen-paneel] 
         [label label]
         [callback (lambda (button event)
                     (set! gekozen-modus modus)
                     (send dialoog show #f))]))

  (maak-knop "Simulator" 'SIM)
  (maak-knop "Hardware (Z21)" 'HARDWARE)  
  (send dialoog show #t) ; blokkeert tot keuze gemaakt is  
  (if gekozen-modus gekozen-modus (exit)))


;;; VIEW LAYER

(define augmented-frame% ;dezelfde als advanced.rkt
  (class frame%
    (init-field [close-callback (thunk (void))])
    (init-rest)
    (super-new)
    (define/augment (on-close) 
      (close-callback))))

(define gui%
  (class object%
    (super-new)
    (init-field
     [voeg-trein-toe-callback DUMMY-CALLBACK]
     [trein-selectie-callback DUMMY-CALLBACK]
     [wijzig-snelheid-callback DUMMY-CALLBACK]
     [stop-trein-callback DUMMY-CALLBACK]
     [verwijder-trein-callback DUMMY-CALLBACK]
     [noodstop-callback DUMMY-CALLBACK]
     [geef-treinen-data (lambda () '())]

     [wissel-selectie-callback DUMMY-CALLBACK]
     [wissel-stand-callback DUMMY-CALLBACK]
     [geef-wissels-data (lambda () '())]

     [slagboom-selectie-callback DUMMY-CALLBACK]
     [slagboom-stand-callback DUMMY-CALLBACK]
     [geef-slagbomen-data (lambda () '())]

     [licht-selectie-callback DUMMY-CALLBACK]
     [licht-stand-callback DUMMY-CALLBACK]
     [geef-lichten-data (lambda () '())]

     [geef-overzicht-data (lambda () "zzz")]

     [laad-scenario-callback DUMMY-CALLBACK]
     [sla-scenario-op-callback DUMMY-CALLBACK]
     )

    ; ===============
    ; HOOFDVENSTER
    ; ===============
    (define hoofd-venster (new augmented-frame%
                               [label "NMBS GUI"]
                               [width VENSTER_BREEDTE]
                               [height VENSTER_HOOGTE]
                               [close-callback (lambda () (displayln "GUI Afgesloten."))]))

    ; =============
    ; TAB PANEL
    ; =============
    (define tab-panel 
      (new tab-panel%
           [parent hoofd-venster]
           [choices '("Trein" "Wissel" "Slagboom" "Verkeerslicht" "Overzicht")]
           [callback (lambda (tp event)
                       (fill-tab! tp (send tp get-selection)))]))

    ; ============
    ; SCENARIO
    ; ============
    (define scenario-paneel 
      (new horizontal-panel% 
           [parent hoofd-venster]
           [alignment '(center center)]
           [spacing SPACING]
           
           ))
           
    (define laad-knop 
      (new button% 
           [parent scenario-paneel] 
           [label "Scenario Inladen"] 
           [callback laad-scenario-callback]))
           
    (define opslaan-knop 
      (new button% 
           [parent scenario-paneel] 
           [label "Scenario Opslaan"] 
           [callback sla-scenario-op-callback]))

    ; ==================
    ; HELPER FUNCTIES
    ; ==================
    
    (define (maak-trein-panel parent)
      (let* ([p1 (new vertical-panel% [parent parent])]
             [titel (new message% [label "Welkom in de Trein sectie."] [parent p1])]
             [trein-lst (new choice% [label "Trein:"] [parent p1] [choices TREIN-IDS])]
             [vorig-lst (new choice% [label "Vorig:"] [parent p1] [choices ALLE-SEGMENTEN])]
             [huidig-lst (new choice% [label "Huidig:"] [parent p1] [choices ALLE-SEGMENTEN])]
             [bestemming-lst (new choice% [label "Bestemming:"] [parent p1] [choices DETECTIEBLOK-IDS])]
             [voeg-toe-knop (new button% [label "Voeg Toe"] [parent p1] [callback voeg-trein-toe-callback])]

             ; Besturen
             [actieve-treinen (new list-box% [label "Actieve Treinen:"] [parent p1] [choices '()]
                                   [callback trein-selectie-callback])]
             [snelheid-slider (new slider% [label "Snelheid:"] [parent p1]
                                   [min-value MIN-SNELHEID] [max-value MAX-SNELHEID] [init-value 0]
                                   [callback wijzig-snelheid-callback])]

             ; Knoppen
             [p2 (new horizontal-panel% [parent p1] [alignment '(center center)])]
             [stop-knop (new button% [label "STOP"] [parent p2] [callback stop-trein-callback])]
             [verwijder-knop (new button% [label "Verwijder"] [parent p2] [callback verwijder-trein-callback])]
             [noodstop-knop (new button% [label "!!! NOODSTOP !!!"] [parent p1] [callback (lambda (b e) (noodstop-callback))] )])
        (trein p1 trein-lst vorig-lst huidig-lst actieve-treinen snelheid-slider bestemming-lst noodstop-knop))) ;str

    (define (maak-wissel-panel parent)
      (let* ([p1 (new vertical-panel% [parent parent])]
             [titel (new message% [label "Wissel Beheer"] [parent p1])]
             [wissel-lst (new choice% [label "Kies Wissel:"] [parent p1] [choices '()] 
                              [callback wissel-selectie-callback])]
             [stand-radio (new radio-box% [label "Stand:"] [parent p1]
                               [choices '("Stand 1" "Stand 2" "Stand 3")] ; 3 standen voor wissel S-2-3
                               [callback wissel-stand-callback])])
        (wissel p1 wissel-lst stand-radio))) ;str

    (define (maak-slagboom-panel parent)
      (let* ([p1 (new vertical-panel% [parent parent])]
             [titel (new message% [label "Slagboom Beheer"] [parent p1])]
             [slagboom-lst (new choice% [label "Kies Slagboom:"] [parent p1] [choices '()] 
                                [callback slagboom-selectie-callback])]
             [positie-radio (new radio-box% [label "Positie:"] [parent p1] [choices '("Open" "Gesloten")] 
                                 [callback slagboom-stand-callback])])
        (slagboom p1 slagboom-lst positie-radio))) ;str

    (define (maak-verkeerslicht-panel parent)
      (let* ([p1 (new vertical-panel% [parent parent])]
             [titel (new message% [label "Verkeerslicht Beheer"] [parent p1])]
             [licht-lst (new choice% [label "Kies Licht:"] [parent p1] [choices '()] 
                             [callback licht-selectie-callback])]
             [lichten-radio (new radio-box% [label "Signaal:"] [parent p1] [choices LICHTEN] 
                                 [callback licht-stand-callback])])
        (verkeerslicht p1 licht-lst lichten-radio))) ;str

    (define (maak-overzicht-panel parent)
      (let* ([p1 (new vertical-panel% [parent parent])]
             [titel (new message% [label "Overzicht"] [parent p1])]
             [inhoud (new message% [label "zzz"] [parent p1] [auto-resize #t])])
        (overzicht p1 inhoud))) ;str

    ; ============================
    ; COMPONENTEN INITIALISEREN
    ; ============================
    (define comp-trein (maak-trein-panel tab-panel))
    (define comp-wissel (maak-wissel-panel tab-panel))
    (define comp-slagboom (maak-slagboom-panel tab-panel))
    (define comp-licht (maak-verkeerslicht-panel tab-panel))
    (define comp-overzicht (maak-overzicht-panel tab-panel))

    ; ==================
    ; FILL-TAB LOGICA
    ; ==================
    (define (fill-tab! tp panel-idx) ;inspiratie: advanced.rkt
      (send tp
            change-children
            (lambda (children)
              (list (case panel-idx
                      [(0) (trein-paneel comp-trein)]
                      [(1) (wissel-paneel comp-wissel)]
                      [(2) (slagboom-paneel comp-slagboom)]
                      [(3) (verkeerslicht-paneel comp-licht)]
                      [(4) (overzicht-paneel comp-overzicht)]))))      
      ; data vernieuwen
      (case panel-idx
        [(0) (vernieuw-treinen-lijst)]
        [(1) (vernieuw-wissels)]
        [(2) (vernieuw-slagbomen)]
        [(3) (vernieuw-lichten)]
        [(4) (vernieuw-overzicht)]))

    (fill-tab! tab-panel 0)


    ; ====================
    ; PUBLIEKE METHODEN
    ; ====================
    (define/public (vernieuw-treinen-lijst)
      (let ([data (geef-treinen-data)]) 
        (send (trein-actieve-lijst comp-trein) set (map symbol->string (map car data)))))
         
    (define/public (vernieuw-wissels)
      (let ([data (geef-wissels-data)]
            [keuze-lijst (wissel-keuze-lijst comp-wissel)])
        (send keuze-lijst clear)
        (for-each (lambda (id)
                    (send keuze-lijst append (symbol->string id)))
                  (map car data))))

    (define/public (vernieuw-slagbomen)
      (let ([data (geef-slagbomen-data)]
            [keuze-lijst (slagboom-keuze-lijst comp-slagboom)])
        (send keuze-lijst clear)
        (for-each (lambda (id)
                    (send keuze-lijst append (symbol->string id)))
                  (map car data))))

    (define/public (vernieuw-lichten)
      (let ([data (geef-lichten-data)]
            [keuze-lijst (verkeerslicht-keuze-lijst comp-licht)])
        (send keuze-lijst clear)
        (for-each (lambda (id)
                    (send keuze-lijst append (symbol->string id)))
                  (map car data))))

    (define/public (vernieuw-overzicht)
      (send (overzicht-msg comp-overzicht) set-label (geef-overzicht-data)))
      
    (define/public (toon-menu) 
      (send hoofd-venster show #t))

    ; Getters
    (define/public (geef-trein) 
      (send (trein-id-keuze comp-trein) get-string-selection))
      
    (define/public (geef-vorig-segment) 
      (send (trein-vorig-keuze comp-trein) get-string-selection))
      
    (define/public (geef-huidig-segment) 
      (send (trein-huidig-keuze comp-trein) get-string-selection))    
    
    (define/public (geef-actieve-trein)
      (send (trein-actieve-lijst comp-trein) get-string-selection))

    (define/public (geef-bestemming)
      (send (trein-bestemming-keuze comp-trein) get-string-selection))
    
    (define/public (set-snelheid-slider! waarde) 
      (send (trein-snelheid-slider comp-trein) set-value waarde))
    
    (define/public (geef-wissel) 
      (send (wissel-keuze-lijst comp-wissel) get-string-selection))
      
    (define/public (set-wissel-selectie! index) 
      (send (wissel-stand-radio comp-wissel) set-selection index))
    
    (define/public (geef-slagboom) 
      (send (slagboom-keuze-lijst comp-slagboom) get-string-selection))
      
    (define/public (set-slagboom-selectie! index) 
      (send (slagboom-positie-radio comp-slagboom) set-selection index))

    (define/public (geef-licht) 
      (send (verkeerslicht-keuze-lijst comp-licht) get-string-selection))
      
    (define/public (set-licht-selectie! index) 
      (send (verkeerslicht-licht-radio comp-licht) set-selection index))
    ))