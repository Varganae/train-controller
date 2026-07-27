#lang racket

(require (prefix-in sim: "simulator/interface.rkt")) 
(require (prefix-in hw: "hardware/interface.rkt"))   

(provide maak-hardware-facade)

(define (maak-hardware-facade modus)  
  ; --- HOGERE ORDE FUNCTIE ---
  (define (kies-in-functie-van-modus sim-proc hw-proc)
    (lambda args
      (cond
        ((eq? modus 'SIM)      (apply sim-proc args))
        ((eq? modus 'HARDWARE) (apply hw-proc args))
        (else (displayln "Error: Ongeldige modus in facade")))))

  ; --- SYSTEEM ---
  (define setup! (kies-in-functie-van-modus sim:setup-hardware void)) ; nog niks voor hardware, dus void
  (define start! (kies-in-functie-van-modus sim:start hw:start))
  (define stop!  (kies-in-functie-van-modus sim:stop  hw:stop))

  ; --- HULPFUNCTIES VOOR WISSEL ---
  (define (sim:veilige-wissel id stand)
    (if (eq? id 'S-2-3)
        (case stand
          ((1) (sim:set-switch-position! 'S-2 1) (sim:set-switch-position! 'S-3 1))
          
          ; S-2 moet op 2 staan om S-3 te bereiken, S-3 staat op 1
          ((2) (sim:set-switch-position! 'S-2 2) (sim:set-switch-position! 'S-3 1))
          
          ; S-2 moet op 2 staan om S-3 te bereiken, S-3 staat op 2
          ((3) (sim:set-switch-position! 'S-2 2) (sim:set-switch-position! 'S-3 2)))
        (sim:set-switch-position! id stand))) 

  (define (sim:geef-veilige-stand id)
    (if (eq? id 'S-2-3)
        (let ((stand2 (sim:get-switch-position 'S-2))
              (stand3 (sim:get-switch-position 'S-3)))
          (if (= stand2 1) ; als S-2 op 1 staat, is het sws pad 1. Anders bepaalt S-3 het pad (2 of 3)
              1
              (if (= stand3 2) 3 2)))
        (sim:get-switch-position id)))

  ; --- COMMANDS ---
  (define set-wissel-stand! 
    (kies-in-functie-van-modus sim:veilige-wissel hw:set-switch-position!))
    
  (define geef-wissel-stand
    (kies-in-functie-van-modus sim:geef-veilige-stand hw:get-switch-position))

  (define registreer-trein! 
    (kies-in-functie-van-modus sim:add-loco hw:add-loco))

  (define verwijder-trein!
    (kies-in-functie-van-modus sim:remove-loco void))

  (define set-trein-snelheid! 
    (kies-in-functie-van-modus sim:set-loco-speed! hw:set-loco-speed!))
  
  (define geef-trein-snelheid 
    (kies-in-functie-van-modus sim:get-loco-speed hw:get-loco-speed))

  (define verander-licht!
    (kies-in-functie-van-modus sim:set-sign-code! hw:set-sign-code!))

  (define open-slagboom! 
    (kies-in-functie-van-modus sim:open-crossing! hw:open-crossing!))

  (define sluit-slagboom! 
    (kies-in-functie-van-modus sim:close-crossing! hw:close-crossing!))

  (define lees-bezette-blokken
    (kies-in-functie-van-modus sim:get-occupied-detection-blocks hw:get-occupied-detection-blocks))

  (lambda (msg . args)
    (case msg
      ('setup! (setup!))
      ('start! (start!))
      ('stop!  (stop!))
      
      ('set-wissel-stand! (set-wissel-stand! (car args) (cadr args)))
      ('geef-wissel-stand (geef-wissel-stand (car args)))

      ('registreer-trein! (registreer-trein! (car args) (cadr args) (caddr args)))
      ('verwijder-trein! (verwijder-trein! (car args)))
      ('set-trein-snelheid! (set-trein-snelheid! (car args) (cadr args)))
      ('geef-trein-snelheid (geef-trein-snelheid (car args)))
      
      ('verander-licht! (verander-licht! (car args) (cadr args)))
      
      ('open-slagboom!  (open-slagboom! (car args)))
      ('sluit-slagboom! (sluit-slagboom! (car args)))
      
      ('lees-bezette-blokken (lees-bezette-blokken))
      
      (else (display "Error in Hardware Facade ADT, msg is: ") (display msg)))))