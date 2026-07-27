#lang racket
(require rackunit
         rackunit/gui
         "../scenario.rkt")

(define (dummy-nmbs msg . args)
  (case msg
    ('geef-alle-wissel-standen '())
    ('geef-slagboom-statussen '())
    ('geef-licht-kleuren '())
    ('geef-alle-trein-ids '())
    (else #t)))

(define TEST-SCENARIO (maak-scenario dummy-nmbs))

(define alle-tests
  (test-suite
   "maak-scenario tests"
   
   (test-case
    "Checkt of een bestand kan worden opgeslagen"
    (TEST-SCENARIO 'sla-scenario-op! "simpel-test.txt")
    (check-true (file-exists? "simpel-test.txt")))

   (test-case
    "Checkt of een bestand kan worden ingeladen"
    (check-not-exn (lambda () (TEST-SCENARIO 'laad-scenario-in! "simpel-test.txt")))
    (when (file-exists? "simpel-test.txt")
      (delete-file "simpel-test.txt")))
   ))

(test/gui alle-tests)