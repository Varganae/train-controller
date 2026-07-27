#lang racket
(provide maak-trein)

(define (maak-trein t-id huidig-segment vorig-segment hardware vorig-voor-sim)
  (let ((huidig huidig-segment)
        (vorig vorig-segment)
        (is-op-spoor? #f)
        (bestemming #f)
        (route-thread #f))

    ; we roepen add-loco aan via de facade
    (hardware 'registreer-trein! t-id vorig-voor-sim huidig)
    (set! is-op-spoor? #t)

    (define (geef-snelheid)
      (hardware 'geef-trein-snelheid t-id))
    
    (define (verwijder-trein!)
      (hardware 'verwijder-trein! t-id)
      (set! is-op-spoor? #f))

    (define (set-snelheid! nieuwe-snelheid)
      (hardware 'set-trein-snelheid! t-id nieuwe-snelheid))

    (define (stop-trein!)
      (hardware 'set-trein-snelheid! t-id 0))    

    (define (geef-positie)
      (list vorig huidig))

    (define (set-bestemming! nieuwe-bestemming)
      (set! bestemming nieuwe-bestemming))

    (lambda (msg . args)
      (case msg
        ('geef-id t-id)
        ('geef-snelheid (geef-snelheid))
        ('verwijder-trein! (verwijder-trein!))
        ('is-op-spoor? is-op-spoor?)        
        ('set-snelheid! (set-snelheid! (car args)))
        ('stop-trein! (stop-trein!))        
        ('geef-positie (geef-positie))
        ('geef-huidig huidig)
        ('set-huidig! (set! huidig (car args)))
        ('geef-vorig vorig)
        ('geef-bestemming bestemming)
        ('set-bestemming! (set-bestemming! (car args)))
        ('geef-route-thread route-thread)
        ('set-route-thread! (set! route-thread (car args)))
        (else (display "Error in trein ADT, msg is: ") (display msg))
        ))
    )
  )