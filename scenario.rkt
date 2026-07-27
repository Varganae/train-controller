#lang racket

(provide maak-scenario)

(define (maak-scenario nmbs-object)

  ; --- HULPPROCEDURES ---
  (define (verzamel-toestand)
    (let* ( (wissel-data (nmbs-object 'geef-alle-wissel-standen))
            (slagboom-data (nmbs-object 'geef-slagboom-statussen))
            (licht-data (nmbs-object 'geef-licht-kleuren))
            (trein-ids (map car (nmbs-object 'geef-alle-trein-ids)))
            (trein-data
             (map (lambda (t-id)
                    (let* ([positie (nmbs-object 'geef-trein-positie t-id)]
                           [vorig-segment (car positie)]
                           [huidig-segment (cadr positie)]
                           [snelheid (nmbs-object 'geef-trein-snelheid t-id)]
                           [bestemming (nmbs-object 'geef-trein-bestemming t-id)])
               
                      (list t-id vorig-segment huidig-segment snelheid bestemming)))
                  trein-ids)))
  
      ; alles in associatielijst zetten
      (list (cons 'opstelling 'simulator) ; hardcoded simulator kiezen
            (cons 'wissels wissel-data)
            (cons 'slagbomen slagboom-data)
            (cons 'lichten licht-data)
            (cons 'treinen trein-data))))

  (define (lees-in pad) ; om bestand veilig te openen
    (call-with-input-file pad read))


  ; --- HOOFDPROCEDURES ---
  (define (sla-scenario-op! pad)
    (call-with-output-file pad
      (lambda (out-port)
        (write (verzamel-toestand) out-port))
      #:exists 'replace))
  
  (define (laad-scenario-in! scenario-data)
    (let 
        (( huidige-treinen (map car (nmbs-object 'geef-alle-trein-ids)))
         ; nodige data
         (wissel-data   (cdr (assoc 'wissels scenario-data)))
         (slagboom-data (cdr (assoc 'slagbomen scenario-data)))
         (licht-data    (cdr (assoc 'lichten scenario-data)))
         (trein-data    (cdr (assoc 'treinen scenario-data))))

      (for-each (lambda (t-id) (nmbs-object 'verwijder-trein! t-id)) huidige-treinen) ; eerst alle andere treinen verwijderen
      
      (for-each (lambda (wissel-info) ; wissels herstellen
                  (nmbs-object 'set-wissel-stand! (car wissel-info) (cadr wissel-info)))
                wissel-data)

       ;---------------------------------------
      (for-each (lambda (boom-info) ; slagbomen herstellen
                  (if (eq? (cadr boom-info) 'Open)
                      (nmbs-object 'open-slagboom! (car boom-info))
                      (nmbs-object 'sluit-slagboom! (car boom-info))))
                slagboom-data)

      (for-each (lambda (licht-info) ; lichten herstellen
                  (nmbs-object 'verander-licht! (car licht-info) (cadr licht-info)))
                licht-data)
  
      (for-each (lambda (trein-info) ; treinen
                  (let ((t-id       (car trein-info))
                        (vorig      (cadr trein-info))
                        (huidig     (caddr trein-info))
                        (snelheid   (cadddr trein-info))
                        (bestemming (car (cddddr trein-info))))
                
                    (nmbs-object 'voeg-trein-toe! t-id huidig vorig)
                    (nmbs-object 'zet-trein-snelheid! t-id snelheid)
                    (when bestemming
                      (nmbs-object 'set-trein-bestemming! t-id bestemming))))
                trein-data)))

  (lambda (msg . args)
    (case msg
      ('sla-scenario-op!  (sla-scenario-op! (car args)))
      ('laad-scenario-in! (laad-scenario-in! (lees-in (car args))))
      (else (display "Error in Scenario ADT, msg is: ") (display msg))
      )))