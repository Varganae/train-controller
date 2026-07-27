#lang racket
(provide maak-verkeerslicht)

(define (maak-verkeerslicht v-id hardware)
  (let ((init 'Hp1)) ; initieel groen
    
    (define (verander-licht! code)
      (hardware 'verander-licht! v-id code)
      (set! init code))
    
    (define (geef-kleur)
      init)

    (lambda (msg . args)
      (case msg
        ('geef-id v-id)
        ('geef-kleur (geef-kleur))
        ('verander-licht! (verander-licht! (car args)))      
        (else (display "Error in Verkeerslicht ADT, msg is: ") (display msg))
        ))
    ))