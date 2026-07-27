#lang racket
(require "execution-facade.rkt")
(require "wissel.rkt")
(require "slagboom.rkt")
(require "verkeerslicht.rkt")
(require "trein.rkt")
(require "constanten.rkt")
(require "spoornetwerk.rkt")
(require "route.rkt")
(require "reservatie.rkt")

(provide maak-infrabel)

(define (zoek-met-id id element-lijst) ;hulpproc
  (cond
    ((null? element-lijst) #f)
    ((eq? ((car element-lijst) 'geef-id) id) (car element-lijst))
    (else (zoek-met-id id (cdr element-lijst)))))

(define (maak-infrabel modus)

  (define hardware (maak-hardware-facade modus)) ; kies de modus

  (let ((wissels '())
        (slagbomen '())
        (verkeerslichten '())
        (treinen '())
        (gecachte-bezette-blokken '())
        (netwerk (maak-spoornetwerk))
        (reservaties (maak-reservatiesysteem)))

    (define (start-bezette-blokken-poller!)
      (thread
       (lambda ()
         (let loop ()
           (define kanaal (make-channel))
           (define t (thread (lambda () (channel-put kanaal (hardware 'lees-bezette-blokken)))))
           (define nieuw (sync/timeout 2.0 kanaal))
           (when nieuw
             (verwerk-positie-veranderingen! gecachte-bezette-blokken nieuw)
             (set! gecachte-bezette-blokken nieuw))
           (kill-thread t)
           (sleep 0.5)
           (loop)))))
    
    (define (verwerk-positie-veranderingen! oud nieuw)
      (define (in-beweging? trein) (not (= 0 (trein 'geef-snelheid))))
      (define nieuw-bezet (filter (lambda (blok) (not (member blok oud))) nieuw))

      (for-each
       (lambda (nieuw-blok)
         (let ((kandidaten (filter (lambda (trein)
                                     (and (netwerk 'adjacent? (trein 'geef-huidig) nieuw-blok)
                                          (in-beweging? trein)))
                                   treinen)))
           (cond
             ((null? kandidaten) (void))
             ((null? (cdr kandidaten)) ; exact 1 kandidaat
              (let ((trein (car kandidaten)))
                (when (not (eq? (trein 'geef-huidig) nieuw-blok))
                  (displayln (format "[~a positie-update] ~a → ~a"
                                     (trein 'geef-id) (trein 'geef-huidig) nieuw-blok))
                  (trein 'set-huidig! nieuw-blok))))
             (else ; 2+ kandidaten
              (displayln (format "[POSITIE-AMBIGU] blok ~a kan van ~a zijn — kies niet"
                                 nieuw-blok (map (lambda (trein) (trein 'geef-id)) kandidaten)))))))
       nieuw-bezet)

      ; botsing op hetzelfde blok
      (let detecteer ((resterende-treinen treinen))
        (unless (null? resterende-treinen)
          (let* ((trein (car resterende-treinen))
                 (huidig-blok (trein 'geef-huidig))
                 (conflicten (filter (lambda (andere-trein)
                                       (and (not (eq? trein andere-trein))
                                            (eq? (andere-trein 'geef-huidig) huidig-blok)))
                                     (cdr resterende-treinen))))
            (when (and (not (null? conflicten))
                       (ormap in-beweging? (cons trein conflicten)))
              (displayln (format "[BOTSING GEDETECTEERD] trein ~a en ~a op blok ~a → noodstop"
                                 (trein 'geef-id)
                                 (map (lambda (andere-trein) (andere-trein 'geef-id)) conflicten)
                                 huidig-blok))
              (noodstop!))
            (detecteer (cdr resterende-treinen)))))

      ; botsing-preventie voor manueel rijden
      (for-each
       (lambda (trein)
         (when (and (in-beweging? trein) (not (trein 'geef-route-thread))) ; manueel = geen route-thread
           (for-each
            (lambda (buur-blok)
              (cond
                ((findf (lambda (andere-trein)
                          (and (not (eq? trein andere-trein)) (eq? (andere-trein 'geef-huidig) buur-blok)))
                        treinen)
                 => (lambda (trein-op-buur)
                      (displayln (format "[VEILIGHEID MANUEEL] bewegende trein ~a heeft buur ~a bezet door ~a → noodstop"
                                         (trein 'geef-id) buur-blok (trein-op-buur 'geef-id)))
                      (noodstop!)))))
            (map car (netwerk 'geef-buren (trein 'geef-huidig))))))
       treinen))

    ; --- RESERVATIE SYSTEEM ---
    (define DBS-LIJST (netwerk 'geef-alle-dbs))
    (define (db? x) (and (memq x DBS-LIJST) #t)) ;memq geeft ofwel lst of #f terug

    (define (keer-nodig? huidig vorig volgend) ;moet de trein zijn rijrichting omkeren om de volgende hop te maken?
      (define resultaat
        (cond
          ((not (and huidig vorig volgend (db? huidig) (db? volgend))) #f)
          ; vorig == volgend: bestemming is precies waar we net vandaan kwamen -> terugdraaien
          ((and (db? vorig) (eq? vorig volgend)) #t)
          ((db? vorig)
           (and (netwerk 'adjacent? huidig vorig) (netwerk 'adjacent? huidig volgend)
                (let ((uit-naar-vorig (or (netwerk 'geef-wissel-instellingen huidig vorig) '()))
                      (uit-naar-volgend (or (netwerk 'geef-wissel-instellingen huidig volgend) '())))
                  (and (not (null? uit-naar-vorig))
                       (not (null? uit-naar-volgend))
                       (let ((w-vorig (car uit-naar-vorig)) (w-volgend (car uit-naar-volgend)))
                         (and (eq? (car w-vorig) (car w-volgend))
                              (not (= (cdr w-vorig) (cdr w-volgend)))))))))
          (else
           (let ((uit-naar-volgend (or (netwerk 'geef-wissel-instellingen huidig volgend) '())))
             (and (not (null? uit-naar-volgend))
                  (eq? vorig (car (car uit-naar-volgend))))))))
      (displayln (format "      [keer?] huidig=~a vorig=~a volgend=~a -> ~a" huidig vorig volgend resultaat))
      resultaat)

    (define (voer-route-uit! t-id pad)
      (displayln (format "[~a route start] pad=~a vorig=~a" t-id pad ((zoek-met-id t-id treinen) 'geef-vorig)))
      (let ((trein (zoek-met-id t-id treinen)))
        (let rijd ((stappen pad)
                   (vorige-res (list (car pad)))
                   (vorig-db (trein 'geef-vorig))
                   (richting 1)) ;1 = vooruit, -1= achteruit
          (cond
            ((null? (cdr stappen)) ; klaar
             (stop-trein! t-id)
             (trein 'set-route-thread! #f)
             (displayln (format "[~a route klaar] eindigde op ~a, reservaties behouden=~a"
                                t-id (car stappen) vorige-res)))
            (else ;niet klaar
             (let* ((van  (car stappen))
                    (naar (cadr stappen))
                    (keren? (keer-nodig? van vorig-db naar))
                    (nieuwe-richting (if keren? (- richting) richting))
                    (inst-heen (or (netwerk 'geef-wissel-instellingen van naar) '()))
                    (inst-terug (or (netwerk 'geef-wissel-instellingen naar van) '()))
                    (inst (append inst-heen
                                  (filter (lambda (wissel-paar) (not (assq (car wissel-paar) inst-heen)))
                                          inst-terug)))
                    (wissel-ids (map car inst))
                    (te-res (cons naar wissel-ids)))
               (displayln (format "[~a hop ~a->~a] keren?=~a richting=~a wissels=~a"
                                  t-id van naar keren? nieuwe-richting inst))
               (let wacht ()
                 (cond
                   ((reservaties 'reserveer! t-id te-res)
                    (displayln (format "[~a hop ~a->~a] reservatie OK voor ~a"
                                       t-id van naar te-res))
                    (for-each (lambda (p)
                                (set-wissel-stand!-intern (car p) (cdr p))
                                (displayln (format "[~a hop ~a->~a] wissel ~a -> stand ~a"
                                                   t-id van naar (car p) (cdr p))))
                              inst)
                    (displayln (format "[~a hop ~a->~a] zet snelheid ~a"
                                       t-id van naar (* nieuwe-richting RIJSNELHEID)))
                    (zet-trein-snelheid! t-id (* nieuwe-richting RIJSNELHEID))
                    (let wacht-aankomst ()
                      (sleep 0.3)
                      (displayln (format "[~a hop ~a->~a] wacht op ~a, bezet=~a"
                                         t-id van naar naar gecachte-bezette-blokken))
                      (cond
                        ((member naar gecachte-bezette-blokken)
                         (displayln (format "[~a hop ~a->~a] aangekomen op ~a"
                                            t-id van naar naar))
                         (trein 'set-huidig! naar)
                         (reservaties 'geef-vrij! vorige-res)
                         (rijd (cdr stappen) te-res van nieuwe-richting))
                        (else (wacht-aankomst)))))
                   (else
                    ; reservatie geweigerd -> stop de trein
                    (stop-trein! t-id)
                    (sleep 0.5)
                    (wacht))))) )))))

    (define (initialiseer-spoornetwerk!)
      (hardware 'setup!)
      (hardware 'start!)

      (let ((wissel-ids (map string->symbol WISSEL-IDS))
            (slagboom-ids (map string->symbol SLAGBOOM-IDS))
            (licht-ids (map string->symbol LICHT-IDS)))

        (set! wissels
              (map (lambda (id) (maak-wissel id hardware)) wissel-ids))

        (set! slagbomen
              (map (lambda (id) (maak-slagboom id hardware)) slagboom-ids))

        (set! verkeerslichten
              (map (lambda (id) (maak-verkeerslicht id hardware)) licht-ids))
        )
      (start-bezette-blokken-poller!)
      )

    ; --- TREINEN ---
    (define (vorig-voor-simulator huidig vorig)
      (let ((inst (or (netwerk 'geef-wissel-instellingen vorig huidig) '())))
        (if (null? inst)
            vorig
            (car (last inst)))))

    (define (voeg-trein-toe! t-id huidig vorig)
      (let ((bestaande-ids (map car (geef-alle-trein-ids))))
        (cond
          ((member t-id bestaande-ids)
           (displayln (format "[~a TOEVOEGEN MISLUKT] id bestaat al" t-id))
           #f)
          ((eq? huidig vorig)
           (displayln (format "[~a TOEVOEGEN MISLUKT] huidig==vorig (~a)" t-id huidig))
           #f)
          ; zonder graaf-adjacentie kent het systeem de oriëntatie niet
          ((not (netwerk 'adjacent? huidig vorig))
           (displayln (format "[~a TOEVOEGEN MISLUKT] vorig=~a is geen graaf-buur van huidig=~a — oriëntatie onbekend"
                              t-id vorig huidig))
           #f)
          (else
           (let* ((vorig-sim (vorig-voor-simulator huidig vorig))
                  (nieuwe-trein (maak-trein t-id huidig vorig hardware vorig-sim)))
             (set! treinen (cons nieuwe-trein treinen))
             (displayln (format "[~a toegevoegd] huidig=~a vorig=~a (sim previous-segment=~a)"
                                t-id huidig vorig vorig-sim)))))))

    (define (verwijder-trein! t-id)
      (let ((trein (zoek-met-id t-id treinen)))
        (if trein
            (begin
              (let ((th (trein 'geef-route-thread)))
                (when th
                  (kill-thread th)))
              (reservaties 'verwijder-trein! t-id)
              (trein 'verwijder-trein!)
              (set! treinen (filter (lambda (t) (not (eq? t trein))) treinen))
              (display "Infrabel: Trein ") (display t-id) (display " verwijderd."))
            (begin
              (display "Error: Trein met id ") (display t-id) (display " niet gevonden")))))

    (define (zet-trein-snelheid! t-id snelheid)
      (let ((trein (zoek-met-id t-id treinen)))
        (if trein
            (trein 'set-snelheid! snelheid)
            (begin
              (display "Error: Trein met id ") (display t-id) (display " niet gevonden")))))

    (define (stop-trein! t-id)
      (let ((trein (zoek-met-id t-id treinen)))
        (if trein
            (trein 'stop-trein!)
            (begin
              (display "Error: Trein met id ") (display t-id) (display " niet gevonden")))))

    (define (geef-trein-snelheid t-id)
      (let ((trein (zoek-met-id t-id treinen)))
        (if trein
            (trein 'geef-snelheid)
            (begin
              (display "Error: Trein met id ") (display t-id) (display " niet gevonden")))))

    (define (geef-trein-positie t-id)
      (let ((trein (zoek-met-id t-id treinen)))
        (if trein
            (trein 'geef-positie)
            #f)))

    (define (noodstop!)
      (for-each (lambda (trein)
                  (trein 'stop-trein!)
                  (let ((th (trein 'geef-route-thread)))
                    (when th
                      (kill-thread th)
                      (trein 'set-route-thread! #f))))
                treinen)
      (reservaties 'wis-alles!)
      (displayln "[NOODSTOP GEACTIVEERD]"))

    (define (geef-trein-bestemming t-id)
      (let ((trein (zoek-met-id t-id treinen)))
        (if trein
            (trein 'geef-bestemming)
            (begin
              (display "Error: Trein met id ") (display t-id) (display " niet gevonden")))))

    (define (set-trein-bestemming! t-id bestemming)
      (displayln (format "[~a bestemming aangevraagd] -> ~a" t-id bestemming))
      (let ((trein (zoek-met-id t-id treinen)))
        (if trein
            (let* ((huidig (trein 'geef-huidig))
                   (resultaat (zoek-route huidig bestemming)))
              (if resultaat
                  (let ((pad (car resultaat)))
                    (let ((th (trein 'geef-route-thread)))
                      (when th
                        (kill-thread th)
                        (trein 'set-route-thread! #f)
                        (reservaties 'verwijder-trein! t-id)))
                    (trein 'set-bestemming! bestemming)
                    (trein 'set-route-thread!
                           (thread (lambda ()
                                     ; wacht tot startblok vrij is, dan voer route uit
                                     (let probeer-start ()
                                       (cond
                                         ((reservaties 'reserveer! t-id (list huidig))
                                          (voer-route-uit! t-id pad))
                                         (else
                                          (displayln (format "[~a START WACHT] startblok ~a bezet, probeer opnieuw..."
                                                             t-id huidig))
                                          (sleep 0.5)
                                          (probeer-start))))))))
                  (displayln (format "[~a ROUTE NIET GEVONDEN] van ~a naar ~a" t-id huidig bestemming))))
            (displayln (format "[~a NIET GEVONDEN]" t-id)))))


    ; --- WISSELS ---
    (define (set-wissel-stand!-intern w-id positie)
      (let ((wissel (zoek-met-id w-id wissels)))
        (if wissel
            (wissel 'set-stand! positie)
            (begin (display "Error: Wissel met id ") (display w-id) (display " niet gevonden")))))

    ; Manueel (GUI)
    (define (set-wissel-stand! w-id positie)
      (let ((bezetter (reservaties 'voer-uit-als-vrij! w-id
                              (lambda () (set-wissel-stand!-intern w-id positie)))))
        (if bezetter ;weiger als wissel door een andere trein gereserveerd is
            (begin
              (displayln (format "[MANUEEL set-wissel-stand! GEWEIGERD] wissel ~a gereserveerd door trein ~a"
                                 w-id bezetter))
              #f)
            #t)))

    (define (geef-wissel-stand w-id)
      (let ((wissel (zoek-met-id w-id wissels)))
        (if wissel
            (wissel 'geef-wissel-stand)
            (begin
              (display "Error: Wissel met id ") (display w-id) (display " niet gevonden")))))

    ; --- SLAGBOMEN ---
    (define (open-slagboom! s-id)
      (let ((slagboom-adt (zoek-met-id s-id slagbomen)))
        (if slagboom-adt
            (slagboom-adt 'open-slagboom!)
            (begin
              (display "Error: Slagboom met id ") (display s-id) (display " niet gevonden")))))

    (define (sluit-slagboom! s-id)
      (let ((slagboom-adt (zoek-met-id s-id slagbomen)))
        (if slagboom-adt
            (slagboom-adt 'sluit-slagboom!)
            (begin
              (display "Error: Slagboom met id ") (display s-id) (display " niet gevonden")))))

    (define (geef-slagboom-status s-id)
      (let ((slagboom-adt (zoek-met-id s-id slagbomen)))
        (if slagboom-adt
            (slagboom-adt 'geef-status)
            (begin
              (display "Error: Slagboom met id ") (display s-id) (display " niet gevonden")))))

    ; --- LICHTEN ---
    (define (verander-licht! v-id code)
      (let ((licht (zoek-met-id v-id verkeerslichten)))
        (if licht
            (licht 'verander-licht! code)
            (begin
              (display "Error: Verkeerslicht met id ") (display v-id) (display " niet gevonden")))))

    (define (geef-lichtkleur v-id)
      (let ((licht (zoek-met-id v-id verkeerslichten)))
        (if licht
            (licht 'geef-kleur)
            (begin
              (display "Error: Verkeerslicht met id ") (display v-id) (display " niet gevonden")))))

    ; --- DATA VOOR GUI ---
    (define (geef-alle-trein-ids)
      (map (lambda (trein)
             (list (trein 'geef-id)))
           treinen))

    (define (geef-alle-wissel-standen)
      (map (lambda (wissel)
             (list (wissel 'geef-id)
                   (wissel 'geef-wissel-stand)))
           wissels))

    (define (geef-slagboom-statussen)
      (map (lambda (slagboom)
             (list (slagboom 'geef-id)
                   (slagboom 'geef-status)))
           slagbomen))

    (define (geef-licht-kleuren)
      (map (lambda (licht-adt)
             (list (licht-adt 'geef-id)
                   (licht-adt 'geef-kleur)))
           verkeerslichten))


    (lambda (msg . args)
      (case msg
        ('initialiseer-spoornetwerk! (initialiseer-spoornetwerk!))

        ('voeg-trein-toe! (voeg-trein-toe! (car args) (cadr args) (caddr args)))
        ('verwijder-trein! (verwijder-trein! (car args)))
        ('zet-trein-snelheid! (zet-trein-snelheid! (car args) (cadr args)))
        ('geef-trein-snelheid (geef-trein-snelheid (car args)))
        ('stop-trein! (stop-trein! (car args)))
        ('geef-trein-positie (geef-trein-positie (car args)))
        ('geef-trein-bestemming (geef-trein-bestemming (car args)))
        ('set-trein-bestemming! (set-trein-bestemming! (car args) (cadr args)))
        ('noodstop! (noodstop!))

        ('set-wissel-stand! (set-wissel-stand! (car args) (cadr args)))

        ('open-slagboom! (open-slagboom! (car args)))
        ('sluit-slagboom! (sluit-slagboom! (car args)))
        ('geef-slagboom-status (geef-slagboom-status (car args)))

        ('verander-licht! (verander-licht! (car args) (cadr args)))
        ('geef-lichtkleur (geef-lichtkleur (car args)))

        ('geef-wissel-stand (geef-wissel-stand (car args)))
        ('geef-alle-wissel-standen (geef-alle-wissel-standen))
        ('geef-alle-trein-ids (geef-alle-trein-ids))
        ('geef-slagboom-statussen (geef-slagboom-statussen))
        ('geef-licht-kleuren (geef-licht-kleuren))

        ('geef-bezette-detectieblokken gecachte-bezette-blokken)

        (else (display "Error in Infrabel ADT, msg is: ") (display msg))
        ))
    ))
