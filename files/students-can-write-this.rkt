;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname students-can-write-this) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require "../main.rkt")

;; ===================================================================================================
;; a dumb student program 

(define (client-their-job _)
  (big-bang #false
    [register   LOCALHOST]
    [to-draw    show]
    [on-receive (λ (w msg) (make-package (play-sound (song-bytes-mp3 msg)) 'next))]))

(define (show w)
  (if (boolean? w) (text "waiting... waiting... waiting..." 22 "red") (text w 22 "black")))

;; ===================================================================================================
(client-their-job 0)

