#lang racket
(require rackunit
         rackunit/gui
         "../spoornetwerk.rkt")


(define TEST-NETWERK (maak-spoornetwerk))

(define spoornetwerk-nieuw-tests
  (test-suite
   "maak-spoornetwerk tests"
   (test-case
    "Checkt of maak-spoornetwerk bestaat"
    (check-not-exn (lambda () maak-spoornetwerk)))

   (test-case
    "Checkt of het maken van het object werkt"
    (check-not-exn (lambda () (maak-spoornetwerk))))))

(define spoornetwerk-geef-alle-dbs-tests
  (test-suite
   "Test op geef-alle-dbs"

   (test-case
    "Checkt of geef-alle-dbs een lijst teruggeeft"
    (check-pred list? (TEST-NETWERK 'geef-alle-dbs)))

   (test-case
    "Checkt of geef-alle-dbs alle 16 detectieblokken bevat"
    (check-eq? (length (TEST-NETWERK 'geef-alle-dbs)) 16))
   ))

(define spoornetwerk-adjacent-tests
  (test-suite
   "Test op adjacent?"

   (test-case
    "Checkt of adjacent? #t teruggeeft voor bestaande edge (1-1 -> 1-7)"
    (check-true (TEST-NETWERK 'adjacent? '1-1 '1-7)))

   (test-case
    "Checkt of adjacent? #t teruggeeft in beide richtingen voor symmetrische edges"
    (check-true (TEST-NETWERK 'adjacent? '1-1 '1-7))
    (check-true (TEST-NETWERK 'adjacent? '1-7 '1-1)))

   (test-case
    "Checkt of adjacent? #f teruggeeft voor niet-bestaande edge (1-1 -> 1-2)"
    (check-false (TEST-NETWERK 'adjacent? '1-1 '1-2)))

   (test-case
    "Checkt of adjacent? #t teruggeeft voor edge via Y-splitsing (1-1 -> 2-4)"
    (check-true (TEST-NETWERK 'adjacent? '1-1 '2-4)))))

(define spoornetwerk-geef-buren-tests
  (test-suite
   "Test op geef-buren"

   (test-case
    "Checkt of geef-buren een lijst teruggeeft"
    (check-pred list? (TEST-NETWERK 'geef-buren '1-1)))
   ))

(define spoornetwerk-geef-wissel-instellingen-tests
  (test-suite
   "Test op geef-wissel-instellingen"

   (test-case
    "Checkt of geef-wissel-instellingen correcte wissels teruggeeft (1-1 -> 1-7 = S-28)"
    (check-equal? (TEST-NETWERK 'geef-wissel-instellingen '1-1 '1-7)
                  '((S-28 . 1))))

   (test-case
    "Checkt of geef-wissel-instellingen meerdere wissels teruggeeft voor langere edge (1-8 -> 2-7)"
    (check-equal? (TEST-NETWERK 'geef-wissel-instellingen '1-8 '2-7)
                  '((S-25 . 2) (S-2-3 . 3) (S-8 . 2) (S-4 . 2))))
   ))

(define alle-tests (test-suite "Alle spoornetwerk operaties tests"
                               spoornetwerk-nieuw-tests
                               spoornetwerk-geef-alle-dbs-tests
                               spoornetwerk-adjacent-tests
                               spoornetwerk-geef-buren-tests
                               spoornetwerk-geef-wissel-instellingen-tests))

(test/gui alle-tests)
