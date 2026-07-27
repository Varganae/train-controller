#lang racket/gui
(require racket/gui/base)
(require "nmbs.rkt")
(require "gui.rkt")
(require "constanten.rkt")
(require "scenario.rkt")

;; =========================================================================
;; VERBINDING MAKEN MET SERVER
;; =========================================================================
(define-values (in-port out-port) (tcp-connect "localhost" 9883)) ; tcp-connect pauzeert tot de server gevonden is
(displayln "Verbonden met Infrabel Server!")

(define mijn-nmbs (maak-nmbs in-port out-port)) ;ipv infrabel object vroeger, nu TCP poort doorgeven

;; =========================================================================
;; START PROGRAMMA
;; =========================================================================

(define (start-program nmbs-object)
  (define scenario-beheerder (maak-scenario nmbs-object))
  (define (wrap-callback f refresh-thunk)
    (lambda (t e) (f t e) (refresh-thunk)))

  ;; --- SCENARIO ACTIES ---
  (define (laad-scenario-actie b e)
    (define gekozen-bestand
      (get-file))
    (when gekozen-bestand
      (scenario-beheerder 'laad-scenario-in! gekozen-bestand)))

  (define (sla-scenario-op-actie b e)
    (define gekozen-bestand
      (put-file #f #f #f #f "txt"))
    (when gekozen-bestand
      (scenario-beheerder 'sla-scenario-op! gekozen-bestand)))

  ;; --- GUI AANMAAK ---
  (define nmbs-gui
    (new gui%
         ; TREIN
         [geef-treinen-data (lambda () (nmbs-object 'geef-alle-trein-ids))]
         [voeg-trein-toe-callback
          (wrap-callback
           (lambda (b e)
             (let ([trein-id    (string->symbol (send nmbs-gui geef-trein))]
                   [huidig-id   (string->symbol (send nmbs-gui geef-huidig-segment))]
                   [vorig-id    (string->symbol (send nmbs-gui geef-vorig-segment))]
                   [bestemming  (string->symbol (send nmbs-gui geef-bestemming))])
               (if (nmbs-object 'voeg-trein-toe! trein-id huidig-id vorig-id)
                   (nmbs-object 'set-trein-bestemming! trein-id bestemming)
                   (message-box "NMBS Operatie Mislukt"
                                "De trein kon niet worden toegevoegd. Controleer de verbindingen of de trein-ID."
                                #f
                                '(ok stop)))))
           (lambda () (send nmbs-gui vernieuw-treinen-lijst)))]
         [trein-selectie-callback (lambda (l e) 
                                    (let ((s (send nmbs-gui geef-actieve-trein)))
                                      (when s (send nmbs-gui set-snelheid-slider!
                                                    (nmbs-object 'geef-trein-snelheid (string->symbol s))))))]
         [wijzig-snelheid-callback (lambda (s e)
                                     (let ((tr (send nmbs-gui geef-actieve-trein)))
                                       (when tr (nmbs-object 'zet-trein-snelheid! 
                                                             (string->symbol tr) (send s get-value)))))]
         [stop-trein-callback (lambda (b e)
                                (let ((tr (send nmbs-gui geef-actieve-trein)))
                                  (when tr (nmbs-object 'stop-trein! (string->symbol tr))
                                    (send nmbs-gui set-snelheid-slider! 0))))]
         [verwijder-trein-callback (wrap-callback 
                                    (lambda (b e)
                                      (let ((tr (send nmbs-gui geef-actieve-trein)))
                                        (when tr (nmbs-object 'verwijder-trein! (string->symbol tr)))))
                                    (lambda () (send nmbs-gui vernieuw-treinen-lijst)))]
             
         ; WISSEL
         [geef-wissels-data (lambda () (nmbs-object 'geef-alle-wissel-standen))]
         [wissel-selectie-callback (lambda (c e)
                                     (let ((w (send nmbs-gui geef-wissel)))
                                       (when w (send nmbs-gui set-wissel-selectie!
                                                     (sub1 (nmbs-object 'geef-wissel-stand (string->symbol w)))))))]
         [wissel-stand-callback 
          (lambda (r e)
            (let ((w (send nmbs-gui geef-wissel)))
              (when w 
                (let ((stand (add1 (send r get-selection))))                  
                  (if (and (not (string=? w "S-2-3")) (= stand 3)) ; controleer of we stand 3 selecteren op een wissel die NIET S-2-3 is
                      (begin
                        (message-box "Fout" "Deze wissel heeft slechts 2 standen!")
                        (send nmbs-gui set-wissel-selectie! ; zet de radio-box terug naar de originele stand
                              (sub1 (nmbs-object 'geef-wissel-stand (string->symbol w)))))
                      (nmbs-object 'set-wissel-stand! (string->symbol w) stand))))))] ; anders -> stuur commando door
             
         ; SLAGBOOM
         [geef-slagbomen-data (lambda () (nmbs-object 'geef-slagboom-statussen))]
         [slagboom-selectie-callback (lambda (c e)
                                       (let ((s (send nmbs-gui geef-slagboom)))
                                         (when s (if (eq? (nmbs-object 'geef-slagboom-status (string->symbol s)) 'Open)
                                                     (send nmbs-gui set-slagboom-selectie! 0)
                                                     (send nmbs-gui set-slagboom-selectie! 1)))))]
         [slagboom-stand-callback (lambda (r e)
                                    (let ((s (send nmbs-gui geef-slagboom)))
                                      (when s (if (= (send r get-selection) 0)
                                                  (nmbs-object 'open-slagboom! (string->symbol s))
                                                  (nmbs-object 'sluit-slagboom! (string->symbol s))))))]
             
         ; VERKEERSLICHT
         [geef-lichten-data 
          (lambda () (nmbs-object 'geef-licht-kleuren))]

         [licht-selectie-callback
          (lambda (c e)
            (define licht (string->symbol (send nmbs-gui geef-licht)))
            (when licht              
              (define code (nmbs-object 'geef-lichtkleur licht))
              (define index (case code
                              ('Hp1 0) 
                              ('Hp0 1) 
                              ('Hp0+Sh0 2) 
                              ('Ks1+Zs3 3)
                              ('Ks2 4) 
                              ('Ks2+Zs3 5) 
                              ('Sh1 6) 
                              ('Ks1+Zs3+Zs3v 7)
                              (else 0)))
              (send nmbs-gui set-licht-selectie! index)))]

         [licht-stand-callback
          (lambda (r e)
            (define licht (string->symbol(send nmbs-gui geef-licht)))
            (define stand-index (send r get-selection))
            (when licht
              (define code (cdr (list-ref LICHT-CONFIGURATIE stand-index)))     
              (nmbs-object 'verander-licht! licht code)))]
             
         ; OVERZICHT
         [geef-overzicht-data (lambda ()
                                (let ((aantal-treinen (length (nmbs-object 'geef-alle-trein-ids)))
                                      (bezette-blokken (nmbs-object 'geef-bezette-detectieblokken)))
                                  (format "Actieve trein(en): ~a.\nBezette detectieblokken: ~a" 
                                          aantal-treinen bezette-blokken)))]

         [noodstop-callback (lambda ()
                              (nmbs-object 'noodstop!)
                              (send nmbs-gui set-snelheid-slider! 0))]

         ; SCENARIO
         [laad-scenario-callback 
          (wrap-callback laad-scenario-actie 
                         (lambda ()
                           (send nmbs-gui vernieuw-overzicht)
                           (send nmbs-gui vernieuw-treinen-lijst)
                           (send nmbs-gui vernieuw-wissels)))]
         
         [sla-scenario-op-callback 
          (wrap-callback sla-scenario-op-actie (lambda () (void)))]

         ))
  
  (new timer%
       [notify-callback
        (lambda ()
          (send nmbs-gui vernieuw-overzicht))]
       [interval 500])
  
  (send nmbs-gui toon-menu))

(start-program mijn-nmbs)

(display "NMBS Client: Gestart.")(newline)