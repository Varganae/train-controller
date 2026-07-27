#lang racket
(require rackunit
         rackunit/gui
         "../wissel.rkt"
         "../execution-facade.rkt")


(define hardware (maak-hardware-facade 'SIM))
(hardware 'setup!)

(define TEST-WISSEL (maak-wissel 'S-1 hardware))

(define wissel-nieuw-tests
  (test-suite
   "maak-wissel tests"
   (test-case
    "Checkt of maak-wissel bestaat"
    (check-not-exn (lambda () maak-wissel)))

   (test-case
    "Checkt of het maken van het object werkt"
    (check-not-exn (lambda () (maak-wissel 'S-1 hardware))))))

(define wissel-methoden-tests
  (test-suite
   "Test op de wissel methoden"
   (test-case
    "Checkt of de stand van de wissel verandert kan worden"
    (TEST-WISSEL 'set-stand! 2)
    (check-eq? (TEST-WISSEL 'geef-wissel-stand) 2))

   (test-case
    "Checkt of de wissel-adt een geldige stand teruggeeft"
    (check-pred number? (TEST-WISSEL 'geef-wissel-stand)))

   (test-case
    "Checkt of de wissel-adt een geldige id teruggeeft"
    (check-pred symbol? (TEST-WISSEL 'geef-id)))
   )
  )

(define alle-tests (test-suite "Alle trein operaties tests"
                               wissel-nieuw-tests
                               wissel-methoden-tests
                               ))
   
(test/gui alle-tests)