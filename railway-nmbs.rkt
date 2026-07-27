#lang racket
(require "constanten.rkt")
(provide maak-railway-nmbs)

(define (maak-railway-nmbs)

  (define wissels
    (map (lambda (id) (list (string->symbol id) 1))
         WISSEL-IDS))

  (define slagbomen
    (map (lambda (id) (list (string->symbol id) 'Open))
         SLAGBOOM-IDS))

  (define lichten
    (map (lambda (id) (list (string->symbol id) 'Hp1))
         LICHT-IDS))

  (define treinen '())

  (define bezette-blokken '())

  ; --- Getters ---
  (define (geef-alle-wissel-standen) wissels)

  (define (geef-wissel-stand w-id)
    (define wissel-info (assq w-id wissels))
    (if wissel-info (cadr wissel-info) #f))

  (define (geef-slagboom-statussen) slagbomen)

  (define (geef-slagboom-status s-id)
    (define slagboom-info (assq s-id slagbomen))
    (if slagboom-info (cadr slagboom-info) #f))

  (define (geef-licht-kleuren) lichten)

  (define (geef-lichtkleur v-id)
    (define lichtkleur-info (assq v-id lichten))
    (if lichtkleur-info (cadr lichtkleur-info) #f))

  (define (geef-alle-trein-ids)
    (map (lambda (t) (list (car t))) treinen))

  (define (geef-trein-snelheid t-id)
    (define trein-info (assq t-id treinen))
    (if trein-info (cadr trein-info) 0))

  (define (geef-bezette-blokken) bezette-blokken)

  ; --- Updaters ---
  (define (update-wissel-stand! w-id stand)
    (set! wissels
          (map (lambda (e) (if (eq? (car e) w-id) (list w-id stand) e))
               wissels)))

  (define (update-slagboom-status! s-id status)
    (set! slagbomen
          (map (lambda (e) (if (eq? (car e) s-id) (list s-id status) e))
               slagbomen)))

  (define (update-lichtkleur! v-id kleur)
    (set! lichten
          (map (lambda (e) (if (eq? (car e) v-id) (list v-id kleur) e))
               lichten)))

  (define (voeg-trein-toe! t-id)
    (unless (assq t-id treinen)
      (set! treinen (cons (list t-id 0) treinen))))

  (define (update-trein-snelheid! t-id snelheid)
    (set! treinen
          (map (lambda (e) (if (eq? (car e) t-id) (list t-id snelheid) e))
               treinen)))

  (define (verwijder-trein! t-id)
    (set! treinen (filter (lambda (e) (not (eq? (car e) t-id))) treinen)))

  (define (noodstop!)
    (set! treinen (map (lambda (e) (list (car e) 0)) treinen)))

  (define (update-bezette-blokken! blokken)
    (set! bezette-blokken blokken))

  (lambda (msg . args)
    (case msg
      ('geef-alle-wissel-standen (geef-alle-wissel-standen))
      ('geef-wissel-stand (geef-wissel-stand (car args)))
      ('geef-slagboom-statussen (geef-slagboom-statussen))
      ('geef-slagboom-status (geef-slagboom-status (car args)))
      ('geef-licht-kleuren (geef-licht-kleuren))
      ('geef-lichtkleur (geef-lichtkleur (car args)))
      ('geef-alle-trein-ids (geef-alle-trein-ids))
      ('geef-trein-snelheid (geef-trein-snelheid (car args)))
      ('geef-bezette-blokken (geef-bezette-blokken))

      ('update-wissel-stand! (update-wissel-stand! (car args) (cadr args)))
      ('update-slagboom-status! (update-slagboom-status! (car args) (cadr args)))
      ('update-lichtkleur! (update-lichtkleur! (car args) (cadr args)))
      ('voeg-trein-toe! (voeg-trein-toe! (car args)))
      ('update-trein-snelheid! (update-trein-snelheid! (car args) (cadr args)))
      ('verwijder-trein! (verwijder-trein! (car args)))
      ('noodstop! (noodstop!))
      ('update-bezette-blokken! (update-bezette-blokken! (car args)))

      (else (display "Error in Railway-NMBS ADT, msg is: ") (display msg))
      ))
  )
