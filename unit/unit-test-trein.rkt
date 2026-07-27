#lang racket
;inspiratie: tutorial
(require rackunit
         rackunit/gui
         "../trein.rkt"
         "../execution-facade.rkt")


(define hardware (maak-hardware-facade 'SIM))
(hardware 'setup!)

(define TEST-TREIN (maak-trein 'T-3 '1-1 '1-2 hardware '1-2))
(define TEST-SNELHEID 50)

(define trein-nieuw-tests
  (test-suite
   "maak-trein tests"
   (test-case
    "Checkt of maak-trein bestaat"
    (check-not-exn (lambda () maak-trein)))

   (test-case
    "Checkt of het maken van het object werkt"
    (check-not-exn (lambda () (maak-trein 'T-3 '1-1 '1-2 hardware '1-2))))))

(define trein-methoden-tests
  (test-suite
   "Test op de trein methoden"
   (test-case
    "Checkt of de snelheid van de trein wordt teruggegeven"
    (TEST-TREIN 'geef-snelheid)
    (check-eq? (TEST-TREIN 'geef-snelheid) 0))

   (test-case
    "Checkt of geef-id het correcte t-id teruggeeft"
    (check-eq? (TEST-TREIN 'geef-id) 'T-3))

   (test-case
    "Checkt of geef-huidig de huidige positie teruggeeft"
    (check-eq? (TEST-TREIN 'geef-huidig) '1-1))

   (test-case
    "Checkt of geef-vorig de vorige positie teruggeeft"
    (check-eq? (TEST-TREIN 'geef-vorig) '1-2))

   (test-case
    "Checkt of geef-positie (vorig huidig) teruggeeft"
    (check-equal? (TEST-TREIN 'geef-positie) '(1-2 1-1)))

   (test-case
    "Checkt of is-op-spoor? #t is na creatie"
    (check-true (TEST-TREIN 'is-op-spoor?)))

   (test-case
    "Checkt of de trein verwijdert kan worden"
    (TEST-TREIN 'verwijder-trein!)
    (check-false (TEST-TREIN 'is-op-spoor?)))

   (test-case
    "Checkt of de snelheid van een trein verandert kan worden"
    (TEST-TREIN 'set-snelheid! TEST-SNELHEID)
    (check-eq? (TEST-TREIN 'geef-snelheid) TEST-SNELHEID))

   (test-case
    "Checkt of de trein stopt"
    (TEST-TREIN 'stop-trein!)
    (check-eq? (TEST-TREIN 'geef-snelheid) 0))

   (test-case
    "Checkt of de bestemming initieel #f is"
    (check-false (TEST-TREIN 'geef-bestemming)))

   (test-case
    "Checkt of de bestemming verandert kan worden"
    (TEST-TREIN 'set-bestemming! '1-8)
    (check-eq? (TEST-TREIN 'geef-bestemming) '1-8))

   (test-case
    "Checkt of set-huidig! de huidige positie kan veranderen"
    (TEST-TREIN 'set-huidig! '2-3)
    (check-eq? (TEST-TREIN 'geef-huidig) '2-3))

   (test-case
    "Checkt of de route-thread initieel #f is"
    (check-false (TEST-TREIN 'geef-route-thread)))

   (test-case
    "Checkt of set-route-thread! een thread kan zetten en geef-route-thread die teruggeeft"
    (define dummy-thread (thread (lambda () (sleep 5))))
    (TEST-TREIN 'set-route-thread! dummy-thread)
    (check-eq? (TEST-TREIN 'geef-route-thread) dummy-thread)
    (kill-thread dummy-thread))

   (test-case
    "Checkt of set-route-thread! op #f gezet kan worden"
    (TEST-TREIN 'set-route-thread! #f)
    (check-false (TEST-TREIN 'geef-route-thread)))

   (test-case
    "Checkt of de trein verwijderd kan worden"
    (TEST-TREIN 'verwijder-trein!)
    (check-false (TEST-TREIN 'is-op-spoor?)))
   )
  )

(define alle-tests (test-suite "Alle trein operaties tests"
                               trein-nieuw-tests
                               trein-methoden-tests
                               ))

(test/gui alle-tests)
