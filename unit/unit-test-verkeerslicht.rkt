#lang racket
(require rackunit
         rackunit/gui
         "../verkeerslicht.rkt"
         "../execution-facade.rkt")


(define hardware (maak-hardware-facade 'SIM))
(hardware 'setup!)

(define TEST-VERKEERSLICHT (maak-verkeerslicht 'L-1 hardware))

(define verkeerslicht-nieuw-tests
  (test-suite
   "maak-verkeerslicht tests"
   (test-case
    "Checkt of maak-verkeerslicht bestaat"
    (check-not-exn (lambda () maak-verkeerslicht)))

   (test-case
    "Checkt of het maken van het object werkt"
    (check-not-exn (lambda () (maak-verkeerslicht 'L-1 hardware))))))

(define verkeerslicht-methoden-tests
  (test-suite
   "Test op de verkeerslicht methoden"
   (test-case
    "Checkt of je de lichten kan veranderen"
    (TEST-VERKEERSLICHT 'verander-licht! 'Hp0)
    (check-eq? (TEST-VERKEERSLICHT 'geef-kleur) 'Hp0))

   (test-case
    "Checkt of de verkeerslicht-adt een geldige kleur teruggeeft"
    (check-pred symbol? (TEST-VERKEERSLICHT 'geef-kleur)))

   (test-case
    "Checkt of de verkeerslicht-adt een geldige id teruggeeft"
    (check-pred symbol? (TEST-VERKEERSLICHT 'geef-id)))
   )
  )

(define alle-tests (test-suite "Alle verkeerslicht operaties tests"
                               verkeerslicht-nieuw-tests
                               verkeerslicht-methoden-tests
                               ))
   
(test/gui alle-tests)