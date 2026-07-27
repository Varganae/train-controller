#lang racket

(require racket/dict)

(require (prefix-in Z21: "Z21/Z21Socket.rkt"))
(require (prefix-in Z21: "Z21/Z21MessageDriving.rkt"))
(require (prefix-in Z21: "Z21/Z21MessageSwitches.rkt"))
(require (prefix-in Z21: "Z21/Z21MessageLocation.rkt"))
(require "Z21/racket-bits-utils-master/bits.rkt")

(provide start
         stop
         
         add-loco
         
         get-loco-speed
         set-loco-speed!
         ;get-loco-detection-block ;; DEPRECATED
         
         get-switch-position
         set-switch-position!

         close-crossing!
         open-crossing!

         set-sign-code!
         
         get-occupied-detection-blocks

         get-detection-block-ids
         get-switch-ids
         
         )

;;;  GLOBALS  ;;;

;; ========================================================================== ;;
;;                                                                            ;;
;; TRAINS                                                                     ;;
;;                                                                            ;;
;; Map the names of the trains (a symbol) to their address.                   ;;
;;                                                                            ;;
;; Note that the trains as represented using symbols (e.g. 'T-3), while       ;;
;; their addresses are numbers (e.g. 3).                                      ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define TRAINS
  #hash((T-3 . 3) (T-5 . 5) (T-7 . 7) (T-9 . 9)))

;; ========================================================================== ;;
;;                                                                            ;;
;; SWITCHES                                                                   ;;
;;                                                                            ;;
;; Map the names of the switches (a symbol) to their address.                 ;;
;;                                                                            ;;
;; Note that the switches as represented using symbols 'S-1 to 'S-28, while   ;;
;; their addresses are numbers 0 to 27.                                       ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define SWITCHES
  #hash((S-1  . 0)
        (S-2  . 1)
        (S-3  . 2)
        (S-4  . 3)
        (S-5  . 4)
        (S-6  . 5)
        (S-7  . 6)
        (S-8  . 7)
        (S-9  . 8)
        (S-10 . 9)
        (S-11 . 10)
        (S-12 . 11)
        (S-16 . 15)
        (S-20 . 19)
        (S-23 . 22)
        (S-24 . 23)
        (S-25 . 24)
        (S-26 . 25)
        (S-27 . 26)
        (S-28 . 27)
        ))

;; ========================================================================== ;;
;;                                                                            ;;
;; (get-switch-ids)                                                           ;;
;;                                                                            ;;
;; Returns a list of all symbols of switch ids.                               ;;
;;                                                                            ;;
;;                                                                            ;;
;; Example usage:                                                             ;;
;; (get-switch-ids)                                                           ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define (get-switch-ids)
  (hash-keys SWITCHES))


;; ========================================================================== ;;
;;                                                                            ;;
;; CROSSING                                                                   ;;
;;                                                                            ;;
;; Map the names of the crossings (a symbol) to their address.                ;;
;;                                                                            ;;
;; Note that the crossings as represented using symbols 'C-1 and 'C-2, while  ;;
;; their addresses are numbers 35 and 36.                                     ;;
;;                                                                            ;;
;; ========================================================================== ;;


(define CROSSINGS
  #hash((C-1 . 35)
        (C-2 . 36)))

;; ========================================================================== ;;
;;                                                                            ;;
;; (get-crossing-ids)                                                         ;;
;;                                                                            ;;
;; Returns a list of all symbols of crossing ids.                             ;;
;;                                                                            ;;
;;                                                                            ;;
;; Example usage:                                                             ;;
;; (get-crossings-ids)                                                        ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define (get-corssing-ids)
  (hash-keys CROSSINGS))

;; ========================================================================== ;;
;;                                                                            ;;
;; SIGNS                                                                     ;;
;;                                                                            ;;
;; Map the names of the signs (a symbol) to a vector of address.              ;;
;; These addresses each have their own meaning namely:                        ;;
;;   1. red (position 1) and green (position 2)                               ;;
;;   2. red + white (position 1) and green + eight (position 2)               ;;
;;   3. orange + white (position 1) and orange + white + 8 (position 2)       ;;
;;   4. white (position 1) and blinking green (position 1) + white + 8 + 6    ;;
;;                                                                            ;;
;; Note that the signs as represented using symbols 'L-1 and 'L-2 , while     ;;
;; their addresses are numbers 40-47.                                         ;;
;;                                                                            ;;
;; ========================================================================== ;;


(define SIGNS
  #hash((L-1 . #(40 41 42 43))
        (L-2 . #(44 45 46 47))))

;; ========================================================================== ;;
;;                                                                            ;;
;; (get-sign-ids)                                                             ;;
;;                                                                            ;;
;; Returns a list of all symbols of sign ids.                                 ;;
;;                                                                            ;;
;;                                                                            ;;
;; Example usage:                                                             ;;
;; (get-sign-ids)                                                             ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define (get-sign-ids)
  (hash-keys SIGNS))

;; ========================================================================== ;;
;;                                                                            ;;
;; DETECTIONBLOCKS                                                            ;;
;;                                                                            ;;
;; A list of all detectionblocks in the hardware.                             ;;
;;                                                                            ;;
;;                                                                            ;;
;;                                                                            ;;
;; ========================================================================== ;;


(define DETECTIONBLOCKS
  '(1-1 1-2 1-3 1-4 1-5 1-6 1-7 1-8 2-1 2-2 2-3 2-4 2-5 2-6 2-7 2-8))

;; ========================================================================== ;;
;;                                                                            ;;
;; (get-detectionblocks-ids)                                                  ;;
;;                                                                            ;;
;; Returns a list of all symbols of detectionblock ids.                       ;;
;;                                                                            ;;
;;                                                                            ;;
;; Example usage:                                                             ;;
;; (get-detectionblock-ids)                                                   ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define (get-detection-block-ids)
  DETECTIONBLOCKS)

  

;;; INTERFACE ;;;

;; ========================================================================== ;;
;;                                                                            ;;
;; (start)                                                                    ;;
;;                                                                            ;;
;; Creates the TCP connection to communicate with the hardware.               ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define (start)
  (set! socket (Z21:setup))
  (Z21:listen socket Z21:handle-msg))

;; ========================================================================== ;;
;;                                                                            ;;
;; (stop)                                                                     ;;
;;                                                                            ;;
;; Stop. When using the real hardware, this doesn't do anything.              ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define (stop)
  (udp-close socket)
  #f)

;; ========================================================================== ;;
;;                                                                            ;;
;; (add-loco TRAIN-ID PREVIOUS-SEGMENT-ID CURRENT-SEGMENT-ID)                 ;;
;;                                                                            ;;
;; Adds a train to the segment with id CURRENT-SEGMENT-ID. The direction of   ;;
;; the train is determined by PREVIOUS-SEGMENT-ID.                            ;;
;;                                                                            ;;
;; This doesn't do anything when using the real hardware: you have to put the ;;
;; train on the tracks yourself :)                                            ;;
;;                                                                            ;;
;; Example usage:                                                             ;;
;; (add-loco 'T-1 'S-27 '1-3)                                                 ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define (add-loco id previous-segment-id current-segment-id)
  #f)

;; ========================================================================== ;;
;;                                                                            ;;
;; (get-loco-speed TRAIN-ID)                                                  ;;
;;                                                                            ;;
;; Returns the current speed of the train with id TRAIN-ID. If the train is   ;;
;; going backwards, returns a negative number.                                ;;
;;                                                                            ;;
;; Example usage:                                                             ;;
;; (get-loco-speed 'T-1)                                                      ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define (get-loco-speed id)
  (let*  ((addr  (dict-ref TRAINS id))
          (lsb   (byte->hex-string addr))
          (msb   "00")
          (msg   (Z21:make-get-loco-info-msg lsb msb))
          (time  (current-inexact-milliseconds))
          (type? Z21:is-loco-info-msg?))
    (Z21:send-msg msg)
    (let* ((resp  (Z21:get-msg type? time))
           (fwd?  (Z21:get-loco-info-forward? resp))
           (speed (Z21:get-loco-info-speed resp)))
      (if fwd? speed (- speed)))))

;; ========================================================================== ;;
;;                                                                            ;;
;; (set-loco-speed! TRAIN-ID SPEED)                                           ;;
;;                                                                            ;;
;; Sets the speed of the train with id TRAIN-ID to SPEED. Setting the         ;;
;; speed to a negative number will make the train go backwards.               ;;
;; The speed can be between -127 and 127.                                     ;;
;;                                                                            ;;
;; Example usage:                                                             ;;
;; (set-loco-speed! 'T-1 100)                                                 ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define (set-loco-speed! id speed)
  (let* ((addr  (dict-ref TRAINS id))
         (lsb   (byte->hex-string addr))
         (msb   "00")
         (fwd?  (> speed 0))
         (range Z21:high-speed-range)
         (speed (abs speed))
         (msg   (Z21:make-set-loco-drive-msg lsb msb range fwd? speed)))
    (Z21:send-msg msg)))

;; ========================================================================== ;;
;;                                                                            ;;
;; (get-loco-detection-block TRAIN-ID)                                        ;;
;;                                                                            ;;
;; Returns the id of the detection block that is currently occupied.          ;;
;; If no detection blocks are occupied, this will return #f.                  ;;
;; Note that the id is ignored: the hardware does not allow you to know which ;;
;; train is on which detection block!                                         ;;
;;                                                                            ;;
;; Example usage:                                                             ;;
;; (get-loco-detection-block 'T-1)                                            ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define (get-loco-detection-block id)
  (let* ((msg   (Z21:make-rmbus-get-data-msg "00"))
         (time  (current-inexact-milliseconds))
         (type? Z21:is-rmbus-datachanged-msg?))
    (Z21:send-msg msg)
    (let loop ((data (Z21:get-rmbus-data (Z21:get-msg type? time))))
      ; data is e.g. '((10) (9) (8) (7) (6) (5) (4) (3) (2) (1 5)) if there's
      ; a train on detection-block 1-5.
      (if (null? data)
          #f
          ; Deconstruct e.g. '(1 5) into module 1, occups '(5)
          (let ((module (number->string (Z21:get-module (car data))))
                (occups (Z21:get-occupancies (car data))))
            (if (null? occups)
                (loop (cdr data))
                ; Only the first result is returned, as the expected return
                ; value of this function is only one detection block.
                (string->symbol (~a module "-" (car occups)))))))))

;; ========================================================================== ;;
;;                                                                            ;;
;; (get-occupied-detection-blocks)                                            ;;
;;                                                                            ;;
;; Returns the ids of the detection blocks that are currently occupied.       ;;
;; If no detection blocks are occupied, this will return '().                 ;;
;; Note: a train may occupy two detection blocks as it is passing from one to ;;
;; the next.                                                                  ;;
;;                                                                            ;;
;; Example usage:                                                             ;;
;; (get-occupied-detection-blocks)                                            ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define (get-occupied-detection-blocks)
  (let* ((msg   (Z21:make-rmbus-get-data-msg "00"))
         (time  (current-inexact-milliseconds))
         (type? Z21:is-rmbus-datachanged-msg?))
    (Z21:send-msg msg)
    (let ((data (Z21:get-rmbus-data (Z21:get-msg type? time))))
      ; data is e.g. '((10) (9) (8) (7) (6) (5) (4) (3) (2 1) (1 5 6)) if
      ; there are trains on blocks 2-1, 1-5, and 1-6.
      (foldl (lambda (module-occups result)
               (let* ((module (Z21:get-module module-occups))
                      (occups (Z21:get-occupancies module-occups))
                      (symbls (map (lambda (occup)
                                     (string->symbol (~a module "-" occup)))
                                   occups)))
                 (append symbls result)))
             '()
             data))))

;; ========================================================================== ;;
;;                                                                            ;;
;; (set-switch-position! SWITCH-ID POSITION)                                  ;;
;;                                                                            ;;
;; Sets the switch with id SWITCH-ID to POSITION. Only 1 and 2 are valid      ;;
;; position numbers. Consult opstelling_schema.pdf to get an overview of all  ;;
;; switch positions. Consult railway.rkt for an overview of all switch ids.   ;;
;;                                                                            ;;
;; When switching the same switch back, you need to wait at least 3 seconds.  ;;                                                                            ;;
;; Example usage:                                                             ;;
;; (set-switch-position! 'S-9 2)                                              ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define (set-switch-position! id position)
  (let* ((addr  (dict-ref SWITCHES id))
         (lsb   (byte->hex-string addr))
         (msb   "00")
         (msg   (Z21:make-set-switch-msg lsb msb #t position)))
    (Z21:send-msg msg)))

(define (set-position! id position)
  (let* ((addr  id)
         (lsb   (byte->hex-string addr))
         (msb   "00")
         (msg   (Z21:make-set-switch-msg lsb msb #t position)))
    (Z21:send-msg msg)))

(define (get-position id)
  (let*  ((addr   id)
          (lsb   (byte->hex-string addr))
          (msb   "00")
          (msg   (Z21:make-get-switch-info-msg lsb msb))
          (time  (current-inexact-milliseconds))
          (type? Z21:is-switch-info-msg?))
    (Z21:send-msg msg)
    (Z21:get-switch-info-position (Z21:get-msg type? time))))


;; ========================================================================== ;;
;;                                                                            ;;
;; (get-switch-position SWITCH-ID)                                            ;;
;;                                                                            ;;
;; Returns the position of the switch with id SWITCH-ID. Consult railway.rkt  ;;
;; for an overview of all switch ids.                                         ;;
;;                                                                            ;;
;; Example usage:                                                             ;;
;; (get-switch-position 'S-9)                                                 ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define (get-switch-position id)
  (let*  ((addr  (dict-ref SWITCHES id))
          (lsb   (byte->hex-string addr))
          (msb   "00")
          (msg   (Z21:make-get-switch-info-msg lsb msb))
          (time  (current-inexact-milliseconds))
          (type? Z21:is-switch-info-msg?))
    (Z21:send-msg msg)
    (Z21:get-switch-info-position (Z21:get-msg type? time))))

;; ========================================================================== ;;
;;                                                                            ;;
;; (close-crossing! CROSSING-ID)                                              ;;
;;                                                                            ;;
;; Closes the barriers of the corssing.                                       ;;
;;                                                                            ;;
;; The closing of the barriers takes up 6 seconds.                            ;;                                                                            
;; Example usage:                                                             ;;
;; (close-crossing! 'C-1)                                                     ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define (close-crossing! id)
  (set-crossing-position! id 2))

;; ========================================================================== ;;
;;                                                                            ;;
;; (open-crossing! CROSSING-ID)                                               ;;
;;                                                                            ;;
;; Opens the barriers of the corssing.                                        ;;
;;                                                                            ;;
;; Opening the barriers takes up 6 seconds.                                   ;;                                                                            
;; Example usage:                                                             ;;
;; (open-crossing! 'C-1)                                                      ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define (open-crossing! id)
  (set-crossing-position! id 1))

;; ========================================================================== ;;
;;                                                                            ;;
;; (set-crossing-position! CROSSING-ID POSITION)                              ;;
;;                                                                            ;;
;; Sets the barriers of the crossing with id CROSSING-ID to POSITION. Only 1  ;;
;; and 2 are valid position numbers. Consult opstelling_schema.pdf to get an  ;;
;; overview of all crossings.                                                 ;;
;;                                                                            ;;
;; When switching the same crossing back, you need to wait at least 6 seconds.;;                                                                            
;; Example usage:                                                             ;;
;; (set-crossing-position! 'C-1 2)                                            ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define (set-crossing-position! id position)
  (let* ((addr  (dict-ref CROSSINGS id))
         (lsb   (byte->hex-string addr))
         (msb   "00")
         ;crossings use the same mechanism as switches
         (msg   (Z21:make-set-switch-msg lsb msb #t position)))
    (Z21:send-msg msg)))


;; =========================================================================== ;;
;;                                                                             ;;
;; (get-crossing-position CROSSING-ID)                                         ;;
;;                                                                             ;;
;; Returns the position of the crossing with id CROSSING-ID.                   ;;
;;                                                                             ;;
;;                                                                             ;;
;; Example usage:                                                              ;;
;; (get-crossing-position 'C-1)                                                ;;
;;                                                                             ;;
;; =========================================================================== ;;

(define (get-crossing-position id)
  (let*  ((addr  (dict-ref CROSSINGS id))
          (lsb   (byte->hex-string addr))
          (msb   "00")
          ;barriers use the same mechanism as switches
          (msg   (Z21:make-get-switch-info-msg lsb msb))
          (time  (current-inexact-milliseconds))
          (type? Z21:is-switch-info-msg?))
    (Z21:send-msg msg)
    (Z21:get-switch-info-position (Z21:get-msg type? time))))


;; ========================================================================== ;;
;;                                                                            ;;
;; (set-sign-code! SIGN-ID SIGNAL)                                      ;;
;;                                                                            ;;
;; Sets the sign with id SIGN-ID to COLOR. Only certain signals are valid  ;;
;; namely:                                                                    ;;
;;     1. Hp0 (red)                                                           ;;
;;     2. Hp1 (green)                                                         ;;
;;     3. Hp0+Sh0 (white - green)                                             ;;
;;     4. Ks1+Zs3 (green - eight)                                             ;;
;;     5. Ks2 (orange - white)                                                ;;
;;     6. Ks2+Zs3 (orange - white - eight)                                    ;;
;;     7. Sh1 (white)                                                         ;;
;;     8. Ks1+Zs3+Zs3v (Blinking green - white - eight -six)                  ;;
;;                                                                            ;;
;; Consult opstelling_schema.pdf to get an overview of all lights             ;;
;;                                                                            ;;
;;                                                                            ;;
;;                                                                            ;;  
;; Example usage:                                                             ;;
;; (set-sign-code! 'L-1 'Hp0)                                                 ;;
;;                                                                            ;;
;; ========================================================================== ;;


(define SIGN-INFO
   #hash((Hp0   . (0 1))
         (Hp1 . (0 2))
         (Hp0+Sh0 . (1 1))
         (Ks1+Zs3 . (1 2))
         (Ks2 . ( 2 1))
         (Ks2+Zs3 . ( 2 2))
         (Sh1 . ( 3 1))
         (Ks1+Zs3+Zs3v . ( 3 2))))

(define get-sign-pos cadr)
(define get-sign-addr car)


(define (set-sign-code! id sign)
  ;Each sign has 4 addresses and each address has two positions. 
  ;Each positions is a different signal meaning that each sign has 8 different signals.
  (let* ((addrs  (dict-ref SIGNS id))
         (sign-info (dict-ref SIGN-INFO sign))
         (addr (vector-ref addrs (get-sign-addr sign-info)))
         (position (get-sign-pos sign-info))
         (lsb   (byte->hex-string addr))
         (msb   "00")
         ;signs use a simular mechanism as switches
         (msg   (Z21:make-set-switch-msg lsb msb #t position)))
    (Z21:send-msg msg)))






;;; UNEXPORTED ;;;

(define socket #f)

(define messages '())

(define (Z21:send-msg msg)
  (Z21:send socket msg)
  (sleep 0.1))

(define (Z21:handle-msg msg)
  (define time (current-inexact-milliseconds))
  (set! messages (cons (cons time msg) messages)))

(define (Z21:get-msg type? request-time)
  (define time car)
  (define content cdr)
  (let loop ((messages messages))
    (if (or (null? messages) (> request-time (time (car messages))))
        (Z21:get-msg type? request-time)
        (if (type? (cdar messages))
            (cdar messages)
            (loop (cdr messages))))))





