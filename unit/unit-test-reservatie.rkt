#lang racket
(require rackunit
         rackunit/gui
         "../reservatie.rkt")


(define TEST-RESERVATIE (maak-reservatiesysteem))

(define reservatie-nieuw-tests
  (test-suite
   "maak-reservatiesysteem tests"
   (test-case
    "Checkt of maak-reservatiesysteem bestaat"
    (check-not-exn (lambda () maak-reservatiesysteem)))

   (test-case
    "Checkt of het maken van het object werkt"
    (check-not-exn (lambda () (maak-reservatiesysteem))))))

(define reservatie-reserveer-tests
  (test-suite
   "Test op reserveer!"

   (test-case
    "Checkt of reserveer! op vrije elementen #t teruggeeft"
    (TEST-RESERVATIE 'wis-alles!)
    (check-true (TEST-RESERVATIE 'reserveer! 'T-3 '(2-4 S-12))))

   (test-case
    "Checkt of reserveer! op elementen al gereserveerd door andere trein #f teruggeeft"
    (TEST-RESERVATIE 'wis-alles!)
    (TEST-RESERVATIE 'reserveer! 'T-3 '(2-4 S-12))
    (check-false (TEST-RESERVATIE 'reserveer! 'T-5 '(2-4 S-12))))))

(define reservatie-geef-vrij-tests
  (test-suite
   "Test op geef-vrij!"

   (test-case
    "Checkt of geef-vrij! elementen vrijgeeft"
    (TEST-RESERVATIE 'wis-alles!)
    (TEST-RESERVATIE 'reserveer! 'T-3 '(2-4 S-12))
    (TEST-RESERVATIE 'geef-vrij! '(2-4 S-12))
    (check-true (TEST-RESERVATIE 'reserveer! 'T-5 '(2-4 S-12))))

   (test-case
    "Checkt of geef-vrij! enkel opgegeven elementen vrijgeeft"
    (TEST-RESERVATIE 'wis-alles!)
    (TEST-RESERVATIE 'reserveer! 'T-3 '(2-4 S-12))
    (TEST-RESERVATIE 'geef-vrij! '(2-4))
    ; 2-4 vrij, S-12 nog door T-3
    (check-true (TEST-RESERVATIE 'reserveer! 'T-5 '(2-4)))
    (check-false (TEST-RESERVATIE 'reserveer! 'T-5 '(S-12))))))

(define reservatie-verwijder-trein-tests
  (test-suite
   "Test op verwijder-trein!"

   (test-case
    "Checkt of verwijder-trein! alle reservaties van een trein wist"
    (TEST-RESERVATIE 'wis-alles!)
    (TEST-RESERVATIE 'reserveer! 'T-3 '(2-4 S-12 S-23))
    (TEST-RESERVATIE 'verwijder-trein! 'T-3)
    (check-true (TEST-RESERVATIE 'reserveer! 'T-5 '(2-4 S-12 S-23))))

   (test-case
    "Checkt of verwijder-trein! enkel deze trein's reservaties wist"
    (TEST-RESERVATIE 'wis-alles!)
    (TEST-RESERVATIE 'reserveer! 'T-3 '(2-4))
    (TEST-RESERVATIE 'reserveer! 'T-5 '(S-12))
    (TEST-RESERVATIE 'verwijder-trein! 'T-3)
    ; T-3's 2-4 vrij, T-5's S-12 niet
    (check-true (TEST-RESERVATIE 'reserveer! 'T-9 '(2-4)))
    (check-false (TEST-RESERVATIE 'reserveer! 'T-9 '(S-12))))))

(define reservatie-wis-alles-tests
  (test-suite
   "Test op wis-alles!"

   (test-case
    "Checkt of wis-alles! alle reservaties wist (van alle treinen)"
    (TEST-RESERVATIE 'wis-alles!)
    (TEST-RESERVATIE 'reserveer! 'T-3 '(2-4))
    (TEST-RESERVATIE 'reserveer! 'T-5 '(S-12))
    (TEST-RESERVATIE 'wis-alles!)
    (check-true (TEST-RESERVATIE 'reserveer! 'T-9 '(2-4 S-12))))))

(define reservatie-eigenaar-tests
  (test-suite
   "Test op eigenaar"

   (test-case
    "Checkt of eigenaar t-id teruggeeft voor gereserveerd element"
    (TEST-RESERVATIE 'wis-alles!)
    (TEST-RESERVATIE 'reserveer! 'T-3 '(2-4))
    (check-eq? (TEST-RESERVATIE 'eigenaar '2-4) 'T-3))))

(define reservatie-voer-uit-als-vrij-tests
  (test-suite
   "Test op voer-uit-als-vrij!"

   (test-case
    "Checkt of voer-uit-als-vrij! thunk uitvoert als element vrij is en #f teruggeeft"
    (TEST-RESERVATIE 'wis-alles!)
    (define uitgevoerd? #f)
    (define resultaat (TEST-RESERVATIE 'voer-uit-als-vrij! 'S-12
                                       (lambda () (set! uitgevoerd? #t))))
    (check-false resultaat)
    (check-true uitgevoerd?))))

(define alle-tests (test-suite "Alle reservatiesysteem operaties tests"
                               reservatie-nieuw-tests
                               reservatie-reserveer-tests
                               reservatie-geef-vrij-tests
                               reservatie-verwijder-trein-tests
                               reservatie-wis-alles-tests
                               reservatie-eigenaar-tests
                               reservatie-voer-uit-als-vrij-tests))

(test/gui alle-tests)
