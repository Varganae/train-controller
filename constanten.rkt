#lang racket
(provide VENSTER_BREEDTE VENSTER_HOOGTE DIALOOG_BREEDTE DIALOOG_HOOGTE SPACING
         RIJSNELHEID ALLE-SEGMENTEN TREIN-IDS
         WISSEL-IDS DETECTIEBLOK-IDS SLAGBOOM-IDS LICHT-IDS
         LICHTEN LICHT-CONFIGURATIE DUMMY-CALLBACK
         MIN-SNELHEID MAX-SNELHEID)

(define VENSTER_BREEDTE 500)
(define VENSTER_HOOGTE 500)
(define DIALOOG_BREEDTE 300)
(define DIALOOG_HOOGTE 200)
(define SPACING 20)

(define RIJSNELHEID 30)

(define TREIN-IDS
  '("T-3" "T-5" "T-7" "T-9"))

(define WISSEL-IDS
  '("S-1" "S-2-3" "S-4" "S-5" "S-6" "S-7" "S-8" "S-9"
    "S-10" "S-11" "S-12" "S-16" "S-20" "S-23" "S-24" "S-25"
    "S-26" "S-27" "S-28"))

(define DETECTIEBLOK-IDS
  '("1-1" "1-2" "1-3" "1-4" "1-5" "1-6" "1-7" "1-8"
    "2-1" "2-2" "2-3" "2-4" "2-5" "2-6" "2-7" "2-8"))

(define SLAGBOOM-IDS '("C-1" "C-2"))

(define LICHT-IDS '("L-1" "L-2"))

(define ALLE-SEGMENTEN
  (append WISSEL-IDS DETECTIEBLOK-IDS))

(define LICHTEN
  '("Groen" "Rood" "Rood-Wit" "Groen-8" "Oranje-Wit" "Oranje-Wit-8" "Wit" "Groen knipper-Wit-8-6"))

(define LICHT-CONFIGURATIE
  '(("Groen" . Hp1)
    ("Rood" . Hp0)
    ("Rood-Wit" . Hp0+Sh0)
    ("Groen-8" . Ks1+Zs3)
    ("Oranje-Wit" . Ks2)
    ("Oranje-Wit-8" . Ks2+Zs3)
    ("Wit" . Sh1)
    ("Groen knipper-Wit-8-6" . Ks1+Zs3+Zs3v)))


(define DUMMY-CALLBACK (lambda (t e) (void)))

(define MIN-SNELHEID -200)
(define MAX-SNELHEID 200)