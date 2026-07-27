#lang racket
(require rackunit
         rackunit/gui
         "../infrabel.rkt")

(define TEST-INFRABEL (maak-infrabel 'SIM))

(TEST-INFRABEL 'initialiseer-spoornetwerk!)

(define infrabel-nieuw-tests
  (test-suite
   "maak-infrabel tests"
   (test-case
    "Checkt of maak-infrabel bestaat"
    (check-not-exn (lambda () maak-infrabel)))

   (test-case
    "Checkt of het maken van het object werkt met modus"
    (check-not-exn (lambda () (maak-infrabel 'SIM))))))

(define infrabel-methoden-tests
  (test-suite
   "Test op de infrabel methoden"

   (test-case
    "Checkt of een trein correct toegevoegd wordt"
    (TEST-INFRABEL 'voeg-trein-toe! 'T-1 '1-1 '1-7)
    (check-eq? (length (TEST-INFRABEL 'geef-alle-trein-ids)) 1))

   (test-case
    "Checkt of Infrabel weigert om een trein met een bestaand ID toe te voegen"
    (check-false (TEST-INFRABEL 'voeg-trein-toe! 'T-1 '1-4 '1-5)))

   (test-case
    "Checkt of Infrabel weigert om een trein toe te voegen met huidig==vorig"
    (check-false (TEST-INFRABEL 'voeg-trein-toe! 'T-7 '1-1 '1-1)))

   (test-case
    "Checkt of Infrabel weigert om een trein toe te voegen met niet-adjacente vorig"
    ; 1-1 en 1-2 zijn geen graaf-buren — oriëntatie onbepaald
    (check-false (TEST-INFRABEL 'voeg-trein-toe! 'T-7 '1-1 '1-2)))

   (test-case
    "Checht of de snelheid van een trein varandert wordt via Infrabel"
    (TEST-INFRABEL 'voeg-trein-toe! 'T-5 '2-1 '2-2)
    (TEST-INFRABEL 'zet-trein-snelheid! 'T-5 100)
    (check-eq? (TEST-INFRABEL 'geef-trein-snelheid 'T-5) 100))

   (test-case
    "Checkt of een trein stopt via Infrabel"
    (TEST-INFRABEL 'stop-trein! 'T-5)
    (check-eq? (TEST-INFRABEL 'geef-trein-snelheid 'T-5) 0))

   (test-case
    "Checkt of positie gegeven wordt via Infrabel"
    (check-equal? (TEST-INFRABEL 'geef-trein-positie 'T-5) '(2-2 2-1)))

   (test-case
    "Checkt of de bestemming initieel #f is voor een nieuwe trein"
    (check-false (TEST-INFRABEL 'geef-trein-bestemming 'T-5)))

   (test-case
    "Checkt of bestemming ingesteld wordt via Infrabel"
    (TEST-INFRABEL 'set-trein-bestemming! 'T-5 '2-5)
    (check-eq? (TEST-INFRABEL 'geef-trein-bestemming 'T-5) '2-5))

   (test-case
    "Checkt of wissels correct aangestuurd worden via Infrabel"
      (TEST-INFRABEL 'set-wissel-stand! 'S-1 2)
      (check-eq? (TEST-INFRABEL 'geef-wissel-stand 'S-1) 2))

   (test-case
    "Checkt of alle wisselstanden opgevraagd kunnen worden"
    (check-pred list? (TEST-INFRABEL 'geef-alle-wissel-standen)))

   (test-case
    "Checkt of slagbomen openen en sluiten via Infrabel"
    (TEST-INFRABEL 'open-slagboom! 'C-1)
    (check-eq? (TEST-INFRABEL 'geef-slagboom-status 'C-1) 'Open)
    (TEST-INFRABEL 'sluit-slagboom! 'C-1)
    (check-eq? (TEST-INFRABEL 'geef-slagboom-status 'C-1) 'Gesloten))

   (test-case
    "Checkt of alle slagboomstatussen opgevraagd kunnen worden"
    (check-pred list? (TEST-INFRABEL 'geef-slagboom-statussen)))

   (test-case
    "Checkt of lichten veranderen en gegeven worden via Infrabel"
    (TEST-INFRABEL 'verander-licht! 'L-1 'Hp0)
    (check-not-false (TEST-INFRABEL 'geef-lichtkleur 'L-1)))

   (test-case
    "Checkt of alle lichtkleuren opgevraagd kunnen worden"
    (check-pred list? (TEST-INFRABEL 'geef-licht-kleuren)))

   (test-case
    "Checkt of de bezette detectieblokken worden doorgegeven"
    (check-pred list? (TEST-INFRABEL 'geef-bezette-detectieblokken)))
   
   ))

(define alle-tests (test-suite "Alle infrabel operaties tests"
                               infrabel-nieuw-tests
                               infrabel-methoden-tests))

(test/gui alle-tests)
