#lang racket
(require rackunit
         rackunit/gui
         "../execution-facade.rkt")

(define TEST-FACADE (maak-hardware-facade 'SIM))
(TEST-FACADE 'setup!)

(define facade-nieuw-tests
  (test-suite
   "maak-hardware-facade tests"
   
   (test-case
    "Checkt of de constructor bestaat"
    (check-not-exn (lambda () maak-hardware-facade)))

   (test-case
    "Checkt of het aanmaken werkt met een geldige modus"
    (check-not-exn (lambda () (maak-hardware-facade 'SIM))))))

(define facade-methoden-tests
  (test-suite
   "Test op de facade methoden"
   (test-case
    "Checkt of het stop! commando zonder problemen passeert"
    (check-not-exn (lambda () (TEST-FACADE 'stop!)))
    )

   (test-case
    "Checkt of een gewone wissel correct wordt doorgegeven"
    (TEST-FACADE 'set-wissel-stand! 'S-1 2)
    (check-eq? (TEST-FACADE 'geef-wissel-stand 'S-1) 2))

   (test-case
    "S-2-3 Logica: Stand 1"
    (TEST-FACADE 'set-wissel-stand! 'S-2-3 1)
    (check-eq? (TEST-FACADE 'geef-wissel-stand 'S-2-3) 1)
    (check-eq? (TEST-FACADE 'geef-wissel-stand 'S-2) 1)
    (check-eq? (TEST-FACADE 'geef-wissel-stand 'S-3) 1))

   (test-case
    "S-2-3 Logica: Stand 2"
    (TEST-FACADE 'set-wissel-stand! 'S-2-3 2)
    (check-eq? (TEST-FACADE 'geef-wissel-stand 'S-2-3) 2)
    (check-eq? (TEST-FACADE 'geef-wissel-stand 'S-2) 2)
    (check-eq? (TEST-FACADE 'geef-wissel-stand 'S-3) 1))

   (test-case
    "S-2-3 Logica: Stand 3"
    (TEST-FACADE 'set-wissel-stand! 'S-2-3 3)
    (check-eq? (TEST-FACADE 'geef-wissel-stand 'S-2-3) 3)
    (check-eq? (TEST-FACADE 'geef-wissel-stand 'S-2) 2)
    (check-eq? (TEST-FACADE 'geef-wissel-stand 'S-3) 2))

   (test-case
    "Checkt of een trein correct geregistreerd kan worden via facade"
    (check-not-exn (lambda () (TEST-FACADE 'registreer-trein! 'T-1 '1-1 '1-2))))

   (test-case
    "Checkt of de treinsnelheid verandert kan worden via facade"
    (TEST-FACADE 'set-trein-snelheid! 'T-1 50)
    (check-eq? (TEST-FACADE 'geef-trein-snelheid 'T-1) 50))

   (test-case
    "Checkt of een trein verwijderd kan worden via facade"
    (check-not-exn (lambda () (TEST-FACADE 'verwijder-trein! 'T-1))))

   (test-case
    "Checkt of lichten, slagbomen en detectie commando's werken"
    (check-not-exn (lambda () (TEST-FACADE 'verander-licht! 'L-1 'Hp0)))
    (check-not-exn (lambda () (TEST-FACADE 'open-slagboom! 'C-1)))
    (check-not-exn (lambda () (TEST-FACADE 'sluit-slagboom! 'C-1)))
    (check-pred list? (TEST-FACADE 'lees-bezette-blokken)))
   
   (test-case
    "Checkt of een ongeldige modus netjes wordt opgevangen"
    (let ((foute-facade (maak-hardware-facade 'ONBESTAANDE-MODUS)))
      (check-not-exn (lambda () (foute-facade 'setup!)))))
   ))

(define alle-tests
  (test-suite "Alle Execution Facade tests"
              facade-nieuw-tests
              facade-methoden-tests))

(test/gui alle-tests)