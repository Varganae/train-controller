#lang racket
(require rackunit
         rackunit/gui
         "../nmbs.rkt")

;; --- HULPFUNCTIE VOOR NETWERK TESTS ---
(define (test-nmbs-commando commando-aanroep verwachte-lijst)
  ; dummy poorten aanmaken
  (let* ((dummy-in-port (open-input-string "#t"))
         (dummy-out-port (open-output-string))
         (TEST-NMBS (maak-nmbs dummy-in-port dummy-out-port)))
    
    (apply TEST-NMBS commando-aanroep)
    (check-equal? (get-output-string dummy-out-port) 
                  (format "~s" verwachte-lijst)))) ; verwachte lijst omzetten naar string zoals write

(define nmbs-nieuw-tests
  (test-suite
   "maak-nmbs tests"
   
   (test-case
    "Checkt of de constructor bestaat"
    (check-not-exn (lambda () maak-nmbs)))

   (test-case
    "Checkt of het aanmaken werkt met virtuele poorten"
    (let ((in (open-input-string ""))
          (out (open-output-string)))
      (check-not-exn (lambda () (maak-nmbs in out)))))))

(define nmbs-netwerk-tests
  (test-suite
   "Test of methodes correct omgezet worden in netwerk-berichten"

   (test-case
    "voeg-trein-toe! stuurt het correcte bericht"
    (test-nmbs-commando '(voeg-trein-toe! T-1 1-1 1-2) 
                        '(voeg-trein-toe! T-1 1-1 1-2)))

   (test-case
    "zet-trein-snelheid! stuurt het correcte bericht"
    (test-nmbs-commando '(zet-trein-snelheid! T-1 50) 
                        '(zet-trein-snelheid! T-1 50)))

   (test-case
    "stop-trein! stuurt het correcte bericht"
    (test-nmbs-commando '(stop-trein! T-5) 
                        '(stop-trein! T-5)))
   
   (test-case
    "verwijder-trein! stuurt het correcte bericht"
    (test-nmbs-commando '(verwijder-trein! T-1) 
                        '(verwijder-trein! T-1)))

   (test-case
    "noodstop! stuurt het correcte bericht"
    (test-nmbs-commando '(noodstop!) 
                        '(noodstop!)))

   (test-case
    "set-wissel-stand! stuurt het correcte bericht"
    (test-nmbs-commando '(set-wissel-stand! S-1 2) 
                        '(set-wissel-stand! S-1 2)))

   (test-case
    "open-slagboom! en sluit-slagboom! sturen correcte berichten"
    (test-nmbs-commando '(open-slagboom! C-1) '(open-slagboom! C-1))
    (test-nmbs-commando '(sluit-slagboom! C-1) '(sluit-slagboom! C-1)))

   (test-case
    "verander-licht! stuurt het correcte bericht"
    (test-nmbs-commando '(verander-licht! L-1 Hp0) 
                        '(verander-licht! L-1 Hp0)))

   (test-case
    "geef-alle-trein-ids stuurt het correcte vraag-bericht"
    (test-nmbs-commando '(geef-alle-trein-ids) 
                        '(geef-alle-trein-ids)))
   
   (test-case
    "geef-trein-positie stuurt het correcte vraag-bericht"
    (test-nmbs-commando '(geef-trein-positie T-1) 
                        '(geef-trein-positie T-1)))
   ))

(define alle-tests
  (test-suite "Alle NMBS Client tests"
              nmbs-nieuw-tests
              nmbs-netwerk-tests))

(test/gui alle-tests)