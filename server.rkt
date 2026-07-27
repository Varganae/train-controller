#lang racket

(require "infrabel.rkt")
(require "gui.rkt")

; INITIALISATIE VAN INFRABEL
(define modus (vraag-gebruiker-modus))
(displayln (format "Infrabel Server start op in modus: ~a..." modus))

(define mijn-infrabel (maak-infrabel modus))
(mijn-infrabel 'initialiseer-spoornetwerk!)

; NETWERK OPZETTEN
(define poort 9883)
(define listener (tcp-listen poort 4 #t))

(thread
 (lambda ()
   (displayln (format "Infrabel luistert op poort ~a. Wachten op NMBS..." poort))
   (define-values (in out) (tcp-accept listener)) ; we maken een thread zodat enkel deze thread wacht op de invoer en niet de hoofdthread
   ;`tcp-accept blokkeert
   ;het wacht passief totdat iemand verbinding maakt. Zolang niemand verbindt, gaat de code niet verder.
   
   ; SERVER LOOP
   (define (server-loop)
     (let ((bericht (read in)))
       (if (eof-object? bericht)
           (begin
             (displayln "Verbinding verbroken door NMBS.")
             (close-input-port in)
             (close-output-port out)
             (tcp-close listener))
           
           (let ((resultaat (apply mijn-infrabel bericht)))
             (define veilig-resultaat 
               (if (void? resultaat) #t resultaat))
               
             (write veilig-resultaat out)
             (newline out)
             (flush-output out)
             (server-loop)))))
   (server-loop))) ; start loop binnen deze thread