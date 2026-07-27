#lang racket
(require "railway-nmbs.rkt")
(provide maak-nmbs)

(define (maak-nmbs in-port out-port)

  (define railway (maak-railway-nmbs))
  (define tcp-mutex (make-semaphore 1))

  ; --- NETWERK PROTOCOL ---
  (define (stuur-bericht bericht)
    (call-with-semaphore tcp-mutex
      (lambda ()
        (write bericht out-port)
        (flush-output out-port)
        (read in-port))))

  ; Achtergrondthread: synct bezette blokken van server (server geeft gecachte waarde terug)
  (thread
   (lambda ()
     (let loop ()
       (define blokken (stuur-bericht '(geef-bezette-detectieblokken)))
       (railway 'update-bezette-blokken! blokken)
       (sleep 0.5)
       (loop))))

  ; --- SETTERS (TCP + lokale update) ---
  (define (voeg-trein-toe! t-id huidig vorig)
    (define succes? (stuur-bericht (list 'voeg-trein-toe! t-id huidig vorig)))
    (when succes? (railway 'voeg-trein-toe! t-id))
    succes?)

  (define (zet-trein-snelheid! t-id snelheid)
    (stuur-bericht (list 'zet-trein-snelheid! t-id snelheid))
    (railway 'update-trein-snelheid! t-id snelheid))

  (define (stop-trein! t-id)
    (stuur-bericht (list 'stop-trein! t-id))
    (railway 'update-trein-snelheid! t-id 0))

  (define (geef-trein-positie t-id)
    (stuur-bericht (list 'geef-trein-positie t-id)))

  (define (noodstop!)
    (stuur-bericht (list 'noodstop!))
    (railway 'noodstop!))

  (define (verwijder-trein! t-id)
    (stuur-bericht (list 'verwijder-trein! t-id))
    (railway 'verwijder-trein! t-id))

  (define (set-wissel-stand! w-id positie)
    (stuur-bericht (list 'set-wissel-stand! w-id positie))
    (railway 'update-wissel-stand! w-id positie))

  (define (open-slagboom! s-id)
    (stuur-bericht (list 'open-slagboom! s-id))
    (railway 'update-slagboom-status! s-id 'Open))

  (define (sluit-slagboom! s-id)
    (stuur-bericht (list 'sluit-slagboom! s-id))
    (railway 'update-slagboom-status! s-id 'Gesloten))

  (define (verander-licht! v-id code)
    (stuur-bericht (list 'verander-licht! v-id code))
    (railway 'update-lichtkleur! v-id code))

  (define (geef-trein-bestemming t-id)
    (stuur-bericht (list 'geef-trein-bestemming t-id)))

  (define (set-trein-bestemming! t-id bestemming)
    (stuur-bericht (list 'set-trein-bestemming! t-id bestemming)))

  ; --- GETTERS (lokaal via Railway-NMBS, geen TCP) ---
  (define (geef-trein-snelheid t-id)
    (railway 'geef-trein-snelheid t-id))

  (define (geef-wissel-stand w-id)
    (railway 'geef-wissel-stand w-id))

  (define (geef-slagboom-status s-id)
    (railway 'geef-slagboom-status s-id))

  (define (geef-lichtkleur v-id)
    (railway 'geef-lichtkleur v-id))

  (define (geef-alle-trein-ids)
    (railway 'geef-alle-trein-ids))

  (define (geef-alle-wissel-standen)
    (railway 'geef-alle-wissel-standen))

  (define (geef-slagboom-statussen)
    (railway 'geef-slagboom-statussen))

  (define (geef-licht-kleuren)
    (railway 'geef-licht-kleuren))

  (define (geef-bezette-detectieblokken)
    (railway 'geef-bezette-blokken))

  ;; --- DISPATCHER ---
  (lambda (msg . args)
    (case msg
      ('voeg-trein-toe! (voeg-trein-toe! (car args) (cadr args) (caddr args)))
      ('zet-trein-snelheid! (zet-trein-snelheid! (car args) (cadr args)))
      ('stop-trein! (stop-trein! (car args)))
      ('geef-trein-positie (geef-trein-positie (car args)))
      ('noodstop! (noodstop!))
      ('verwijder-trein! (verwijder-trein! (car args)))
      ('set-wissel-stand! (set-wissel-stand! (car args) (cadr args)))
      ('open-slagboom! (open-slagboom! (car args)))
      ('sluit-slagboom! (sluit-slagboom! (car args)))
      ('verander-licht! (verander-licht! (car args) (cadr args)))
      ('geef-trein-bestemming (geef-trein-bestemming (car args)))
      ('set-trein-bestemming! (set-trein-bestemming! (car args) (cadr args)))

      ('geef-trein-snelheid (geef-trein-snelheid (car args)))
      ('geef-wissel-stand (geef-wissel-stand (car args)))
      ('geef-slagboom-status (geef-slagboom-status (car args)))
      ('geef-lichtkleur (geef-lichtkleur (car args)))
      ('geef-alle-trein-ids (geef-alle-trein-ids))
      ('geef-alle-wissel-standen (geef-alle-wissel-standen))
      ('geef-slagboom-statussen (geef-slagboom-statussen))
      ('geef-licht-kleuren (geef-licht-kleuren))
      ('geef-bezette-detectieblokken (geef-bezette-detectieblokken))

      (else (display "Error in NMBS ADT, msg is: ") (display msg))
      ))
  )
