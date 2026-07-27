#lang racket
(require "spoornetwerk.rkt"
         "graph/bft-labeled.rkt")
(provide zoek-route)

(define netwerk (maak-spoornetwerk))

;HULPPROCS
(define (verzamel-wissels pad)
  (if (null? (cdr pad))
      '()
      (append (or (netwerk 'geef-wissel-instellingen (car pad) (cadr pad))
                  '())
              (verzamel-wissels (cdr pad)))))

(define aantal-dbs (length (netwerk 'geef-alle-dbs)))

; Geeft #f als geen route bestaat, anders (list pad instellingen)
(define (zoek-route start doel)
  (define start-idx (db->idx start))
  (define doel-idx  (db->idx doel))

  (if (equal? start doel)
      (list (list start) '())
      (let ((parent (make-vector aantal-dbs #f))
            (gevonden #f))

        (bft GRAAF
             root-nop ; geen actie bij start
             (lambda (node lbl) ; elke nieuwe knoop: doel bereikt?
               (if (= node doel-idx)
                   (begin (set! gevonden #t) #f) ; ja: flaggen, stop de tak
                   #t)) ; nee: blijf zoeken
             (lambda (from to lbl) ; elke nieuwe edge: parent onthouden
               (vector-set! parent to from)
               #t)
             edge-nop ; al-bezochte buur: negeren
             (list start-idx)) ; vanaf welke knoop

        (and gevonden ; er moet een pad zijn en ...
             (let* ((pad-idx (reconstrueer start-idx doel-idx parent))
                    (pad (map idx->db pad-idx))
                    (inst (verzamel-wissels pad))) ; wissel-instellingen voor het hele traject
               (list pad inst))))))

(define (reconstrueer start-idx doel-idx parent) ; loopt terug door de parent-vector vanaf doel-idx en bouwt het pad onderweg op
  (let loop ((idx doel-idx)
             (pad '()))
    (if (= idx start-idx)
        (cons start-idx pad)
        (loop (vector-ref parent idx) ;spring naar parent van idx
              (cons idx pad))))) ;prepend huidige idx
