#lang racket
(require (prefix-in g: "graph/config.rkt"))
(require "constanten.rkt")
(provide maak-spoornetwerk GRAAF db->idx idx->db)

(define DBS (map string->symbol DETECTIEBLOK-IDS))

;HULPPROC
(define (db->idx db)
  (let loop ((lst DBS)
             (i 0))
    (cond
      ((null? lst) #f)
      ((eq? (car lst) db) i)
      (else (loop (cdr lst) (+ i 1))))))
(define (idx->db i)  (list-ref DBS i))

(define GRAAF
  (let ((g (g:new #t (length DBS))))
    (let loop ((lst DBS) (i 0))
      (unless (null? lst)
        (g:label! g i (car lst))
        (loop (cdr lst) (+ i 1))))
    (define (e! van naar inst) (g:add-edge! g (db->idx van) (db->idx naar) inst))
    ; ---- 1-1 ----
    (e! '1-1 '1-7 '((S-28 . 1)))
    (e! '1-1 '2-4 '((S-12 . 1)))
    (e! '1-1 '2-3 '((S-12 . 2)))
    ; ---- 1-2 ----
    (e! '1-2 '1-4 '())
    (e! '1-2 '2-4 '((S-9 . 1)))
    (e! '1-2 '2-3 '((S-9 . 2) (S-12 . 2)))
    ; ---- 1-3 ----
    (e! '1-3 '1-4 '())
    (e! '1-3 '2-4 '())
    ; ---- 1-4 ----
    (e! '1-4 '1-5 '())
    (e! '1-4 '1-3 '((S-26 . 2) (S-27 . 1)))
    (e! '1-4 '1-2 '((S-26 . 2) (S-27 . 2)))
    ; ---- 1-5 ----
    (e! '1-5 '1-4 '())
    (e! '1-5 '2-4 '())
    ; ---- 1-6 ----
    (e! '1-6 '1-7 '())
    (e! '1-6 '2-3 '((S-6 . 1)))
    (e! '1-6 '2-4 '((S-6 . 2)))
    ; ---- 1-7 ----
    (e! '1-7 '1-1 '((S-28 . 1)))
    (e! '1-7 '1-6 '())
    ; ---- 1-8 ----
    (e! '1-8 '2-3 '((S-25 . 1) (S-6 . 1)))
    (e! '1-8 '2-4 '((S-25 . 1) (S-6 . 2)))
    (e! '1-8 '2-2 '((S-25 . 2) (S-2-3 . 2)))
    (e! '1-8 '2-5 '((S-25 . 2) (S-2-3 . 3) (S-8 . 1)))
    (e! '1-8 '2-6 '((S-25 . 2) (S-2-3 . 3) (S-8 . 2) (S-4 . 1)))
    (e! '1-8 '2-7 '((S-25 . 2) (S-2-3 . 3) (S-8 . 2) (S-4 . 2)))
    ; ---- 2-1 ----
    (e! '2-1 '2-2 '((S-2-3 . 2)))
    (e! '2-1 '2-3 '((S-2-3 . 1) (S-6 . 1)))
    (e! '2-1 '2-4 '((S-2-3 . 1) (S-6 . 2)))
    (e! '2-1 '2-5 '((S-2-3 . 3) (S-8 . 1)))
    (e! '2-1 '2-6 '((S-2-3 . 3) (S-8 . 2) (S-4 . 1)))
    (e! '2-1 '2-7 '((S-2-3 . 3) (S-8 . 2) (S-4 . 2)))
    ; ---- 2-2 ----
    (e! '2-2 '2-1 '((S-1 . 1)))
    (e! '2-2 '1-8 '((S-1 . 2)))
    ; ---- 2-3 ----
    (e! '2-3 '1-2 '((S-11 . 1)))
    (e! '2-3 '1-1 '((S-11 . 2) (S-10 . 1)))
    (e! '2-3 '1-6 '((S-5 . 1)))
    (e! '2-3 '1-8 '((S-5 . 2) (S-7 . 1)))
    (e! '2-3 '2-1 '((S-5 . 2) (S-7 . 2) (S-1 . 1)))
    ; ---- 2-4 ----
    (e! '2-4 '1-2 '((S-23 . 1) (S-24 . 1)))
    (e! '2-4 '1-3 '((S-23 . 1) (S-24 . 2)))
    (e! '2-4 '1-1 '((S-23 . 2) (S-11 . 2) (S-10 . 1)))
    (e! '2-4 '1-5 '((S-20 . 1)))
    (e! '2-4 '1-6 '((S-20 . 2) (S-5 . 1)))
    (e! '2-4 '1-8 '((S-20 . 2) (S-5 . 2) (S-7 . 1)))
    (e! '2-4 '2-1 '((S-20 . 2) (S-5 . 2) (S-7 . 2) (S-1 . 1)))
    ; ---- 2-5 ----
    (e! '2-5 '2-1 '((S-1 . 1)))
    (e! '2-5 '1-8 '((S-1 . 2)))
    ; ---- 2-6 ----
    (e! '2-6 '2-1 '((S-1 . 1)))
    (e! '2-6 '1-8 '((S-1 . 2)))
    ; ---- 2-7 ----
    (e! '2-7 '2-1 '((S-1 . 1)))
    (e! '2-7 '1-8 '((S-1 . 2)))
    ; ---- 2-8 ----
    (e! '1-1 '2-8 '((S-10 . 1) (S-16 . 2)))
    (e! '2-8 '1-1 '((S-10 . 1) (S-16 . 2)))
    g))

(define (maak-spoornetwerk)
  (define (geef-buren db)
    (let ((res '()))
      (g:for-each-edge GRAAF (db->idx db)
                       (lambda (to lbl) (set! res (cons (cons (idx->db to) lbl) res))))
      res))

  (define (adjacent? van naar)
    (g:adjacent? GRAAF (db->idx van) (db->idx naar)))

  (define (geef-wissel-instellingen van naar)
    (g:edge-label GRAAF (db->idx van) (db->idx naar)))

  (lambda (msg . args)
    (case msg
      ('geef-buren (geef-buren (car args)))
      ('geef-alle-dbs DBS)
      ('adjacent? (adjacent? (car args) (cadr args)))
      ('geef-wissel-instellingen (geef-wissel-instellingen (car args) (cadr args)))
      (else (display "Error in Spoornetwerk ADT, msg is: ") (display msg)))))