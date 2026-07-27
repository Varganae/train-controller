#lang racket

(provide maak-reservatiesysteem)

(define (maak-reservatiesysteem)
  (let ((reservaties '())
        (sem (make-semaphore 1)))

    ; Probeer atomair een set elementen te reserveren voor t-id.
    ; #t = volledige reservatie geslaagd, #f = minstens één conflict, niets veranderd
    (define (reserveer! t-id elementen)
      (call-with-semaphore sem
        (lambda ()
          (let ((conflict? (lambda (e)
                             (let ((p (assq e reservaties)))
                               (and p (not (eq? (cdr p) t-id)))))))
            (if (ormap conflict? elementen)
                #f
                (begin
                  (set! reservaties
                        (append (map (lambda (e) (cons e t-id))
                                     (filter (lambda (e) (not (assq e reservaties)))
                                             elementen))
                                reservaties))
                  #t))))))

    ; Geef een set elementen vrij (verwijder uit reservaties).
    (define (geef-vrij! elementen)
      (call-with-semaphore sem
        (lambda ()
          (set! reservaties
                (filter (lambda (p) (not (memq (car p) elementen))) reservaties)))))

    ; Verwijder alle reservaties van een specifieke trein.
    (define (verwijder-trein! t-id)
      (call-with-semaphore sem
        (lambda ()
          (set! reservaties
                (filter (lambda (p) (not (eq? (cdr p) t-id))) reservaties)))))

    ; Wis alle reservaties (gebruikt door noodstop).
    (define (wis-alles!)
      (call-with-semaphore sem
        (lambda () (set! reservaties '()))))

    ; Geef de t-id van de eigenaar van een element terug, of #f als het vrij is.
    (define (eigenaar e)
      (call-with-semaphore sem
        (lambda ()
          (let ((p (assq e reservaties)))
            (if p (cdr p) #f)))))

    ; Voer thunk uit ENKEL als element vrij is — alles atomair onder dezelfde lock
    ; Returnt #f als thunk werd uitgevoerd
    (define (voer-uit-als-vrij! e thunk)
      (call-with-semaphore sem
        (lambda ()
          (let ((p (assq e reservaties)))
            (if p
                (cdr p)
                (begin (thunk) #f))))))

    (lambda (msg . args)
      (case msg
        ('reserveer! (reserveer! (car args) (cadr args)))
        ('geef-vrij! (geef-vrij! (car args)))
        ('verwijder-trein! (verwijder-trein! (car args)))
        ('wis-alles! (wis-alles!))
        ('eigenaar (eigenaar (car args)))
        ('voer-uit-als-vrij! (voer-uit-als-vrij! (car args) (cadr args)))
        (else (display "Error in Reservatie ADT, msg is: ") (display msg))))))
