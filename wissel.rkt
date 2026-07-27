#lang racket
(provide maak-wissel)

(define (maak-wissel w-id hardware)

  (define stand 1)

  (define (set-stand! positie)
    (set! stand positie)
    (hardware 'set-wissel-stand! w-id positie))

  (define (geef-wissel-stand)
    stand)
  
  (lambda (msg . args)
    (case msg
      ('geef-id w-id)
      ('geef-wissel-stand (geef-wissel-stand))
      ('set-stand! (set-stand! (car args)))
      (else (display "Error in Wissel ADT, msg is: ") (display msg))
  ))
)