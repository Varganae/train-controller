#lang racket
(require rackunit
         rackunit/gui
         "../slagboom.rkt"
         "../execution-facade.rkt")


(define hardware (maak-hardware-facade 'SIM))
(hardware 'setup!)

(define TEST-SLAGBOOM (maak-slagboom 'C-1 hardware))

(define slagboom-nieuw-tests
  (test-suite
   "maak-slagboom tests"
   (test-case
    "Checkt of maak-slagboom bestaat"
    (check-not-exn (lambda () maak-slagboom)))

   (test-case
    "Checkt of het maken van het object werkt"
    (check-not-exn (lambda () (maak-slagboom 'C-1 hardware))))))

(define slagboom-methoden-tests
  (test-suite
   "Test op de slagboom methoden"
   (test-case
    "Checkt of de slagboom geopend kan worden"
    (TEST-SLAGBOOM 'open-slagboom!)
    (check-eq? (TEST-SLAGBOOM 'geef-status) 'Open))

   (test-case
    "Checkt of de slagboom gesloten kan worden"
    (TEST-SLAGBOOM 'sluit-slagboom!)
    (check-eq? (TEST-SLAGBOOM 'geef-status) 'Gesloten))

   (test-case
    "Checkt of de slagboom-adt een geldige status teruggeeft"
    (check-pred symbol? (TEST-SLAGBOOM 'geef-status)))

   (test-case
    "Checkt of de slagboom-adt een geldige id teruggeeft"
    (check-pred symbol? (TEST-SLAGBOOM 'geef-id)))
   )
  )

(define alle-tests (test-suite "Alle trein operaties tests"
                               slagboom-nieuw-tests
                               slagboom-methoden-tests
                               ))
   
(test/gui alle-tests)