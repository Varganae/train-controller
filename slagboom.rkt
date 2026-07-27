#lang racket
(provide maak-slagboom)

(define (maak-slagboom s-id hardware)
  (let ((status 'Open))

    (define (open-slagboom!)
      (hardware 'open-slagboom! s-id)
      (set! status 'Open))

    (define (sluit-slagboom!)
      (hardware 'sluit-slagboom! s-id)
      (set! status 'Gesloten))

    (define (geef-status)
      status)

    (lambda (msg . args)
      (case msg
        ('geef-id s-id)
        ('open-slagboom! (open-slagboom!))
        ('sluit-slagboom! (sluit-slagboom!))
        ('geef-status (geef-status))
        (else (display "Error in Slagboom ADT, msg is: ") (display msg))
        ))
    ))