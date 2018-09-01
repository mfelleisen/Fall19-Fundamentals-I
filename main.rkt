#lang racket/gui

;; ---------------------------------------------------------------------------------------------------
;; the retriev-result function is kind of within reach of students after the first week
;; at the pace of the regular schedue; they should do it so they don't think this is black magic 

(provide

 ;; String -> Response U False 
 self-test 

 ;; type Response <= String is one of
 DONT     ;; feedback has been provided
 LIKE     ;; feedback has been provided
 DONE     ;; the song is over, no feedback 
  
 ;; FilePath -> Bytes
 ;; read a file as bytestring (for example, an MP3 file)
 file-as-bytes

 ;; (-> bytes? response?)
 ;; playing mp3 bytes to get feedback 
 play-sound
 
 ;; the following suggests that transmitted songs are structures
 ;; but they are byte strings and if they play the whole thing before
 ;; stripping the title, it also plays a song right now 

 ;   (-> any/c boolean?)
 song-bytes?
 ;  (-> string? bytes? song-bytes?)
 make-song-bytes
 ;  (-> song-bytes? string?)
 song-bytes-title
 ;    (-> song-bytes? bytes?)
 song-bytes-mp3)
  

;; ---------------------------------------------------------------------------------------------------
;; dependencies 
(require 2htdp/image)
(require 2htdp/universe)
(require htdp/error)
(require (except-in video/base color))
(require video/player)
(require racket/runtime-path)

(module+ test
  (define THISFILE "main.rkt")
  
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(define (file-as-bytes a-path)
  (check-arg 'file-as-bytes (string? a-path) "string" "the" a-path)
  [define a-path! (string->path a-path)]
  (if (file-exists? a-path!)
      (file->bytes a-path)
      (error 'file-as-bytes "not a file: ~e" a-path)))

(module+ test
  (check-exn #px"string as the argument" (lambda () (file-as-bytes 0)))
  (check-exn exn:fail? (lambda () (file-as-bytes "i-do-not-exist.rkt")))
  (check-equal? (regexp-match #px"#lang racket" (file-as-bytes THISFILE)) '(#"#lang racket")))

;; ---------------------------------------------------------------------------------------------------
;; playing an MP3 and providing feedback 

(define DONT "dislike")
(define LIKE "like")
(define DONE "none")

(define-runtime-path PLAY.PNG  "private/play.png")
(define-runtime-path PAUSE.PNG "private/pause.png")
(define-runtime-path LIKE.PNG  "private/like.png")
(define-runtime-path DONT.PNG  "private/dont.png")

(define (play-sound mp3)
  (check-arg 'play-sound (bytes? mp3) "byte string" "the" mp3)
  (define custodian (make-custodian))
  (parameterize ((current-custodian custodian))
    (parameterize ((current-eventspace (make-eventspace)))
      (define vps (mp3->vps mp3))
      (send vps play)
      (define stateN (retrieve-result vps))
      (send vps stop)
      (custodian-shutdown-all custodian)
      (cond
        [(false? stateN)    DONE]
        [(symbol? stateN)   DONE]
        [(response? stateN) stateN]))))

;; Any -> Boolean 
(define (response? x)
  (memq x (list DONT LIKE DONE)))

;; Bytes -> VideoPlayer 
(define (mp3->vps mp3)
  (define file-path (make-temporary-file))
  (with-output-to-file file-path #:exists 'replace (lambda () (write-bytes mp3)))
  (define file (path->string file-path))
  (define vps  (new video-player-server% [video (clip file)]))
  vps)

(define PAUSED 'paused)
;; type State =
;;  | False    ;; the song is playing, no feedback
;;  | PAUSED   ;; the song is paused
;;  U Response

;; -> State 
(define (retrieve-result vps)
  (define gui
    (new gui%  ;; observe contractual obligations
         [paused?  #false]
         [cb-play  (λ (s) (unless (false? s)     (send vps play))  #false)]
         [cb-pause (λ (s) (unless (eq? PAUSED s) (send vps pause)) PAUSED)]
         [cb-like  (λ (_)                                          LIKE)]
         [cb-dont  (λ (_)                                          DONT)]))

  (big-bang #f
    [to-draw   (λ (s) (send gui show (eq? PAUSED s)))]
    [on-tick   (λ (s) (if (and (boolean? s) (send vps is-stopped?)) DONE s))]
    [on-mouse  (λ (s x y me) (cond
                               [(not (mouse=? me "button-down")) s]
                               [(send gui geometry-manager x y) => (λ (cb) (cb s))]
                               [else s]))]
    [stop-when string?]))

;; (X) (class [cb-play (X -> X)] [cb-pause (X -> X)] [cb-like (X -> X)] [cb-dont (X -> X)]
;; produces 
;; (object [show (Boolean -> Image)] [geometry-manager (N N -> (X -> X))])
;; implement the primitive geometry management for buttons 
(define gui%
  (class object% (init paused? cb-play cb-pause cb-like cb-dont)
    
    (define PLAY (scale .25 (bitmap/file PLAY.PNG)))
    (define PAUS (scale .25 (bitmap/file PAUSE.PNG)))
    (define LIKE (scale .25 (bitmap/file LIKE.PNG)))
    (define DONT (scale .25 (bitmap/file DONT.PNG)))

    ;; sizes and shapes 
    (define WIDTH   (image-width PLAY))
    (define 2WIDTH  (* 2 WIDTH))
    (define HEIGHT  (image-height PLAY))
    (define 2HEIGHT (* 2 HEIGHT))
    
    ;; <Image , (X) (X -> X) >
    ;; generate images for buttons and callbacks 
    (define-values (play-button play-clicked) (button 0     2WIDTH 0      HEIGHT  PLAY cb-play))
    (define-values (paus-button paus-clicked) (button 0     2WIDTH 0      HEIGHT  PAUS cb-pause))
    (define-values (like-button like-clicked) (button 0     WIDTH  HEIGHT 2HEIGHT LIKE cb-like))
    (define-values (dont-button dont-clicked) (button WIDTH 2WIDTH HEIGHT 2HEIGHT DONT cb-dont))

    (field [xyz-clicked #false])

    (define/public (show paused?)
      (cond
        [paused?
         (set! xyz-clicked play-clicked)
         (above play-button
                (beside like-button dont-button))]
        [else
         (set! xyz-clicked paus-clicked)
         (above paus-button
                (beside like-button dont-button))]))

    ;; (N N -> (x -> X)
    ;; figure out which of the callbacks may fire, fire it and compute next X 
    (define/public (geometry-manager x y)
      (or (xyz-clicked x y)
          (like-clicked x y)
          (dont-clicked x y)))

    (super-new)
    (show paused?)))

;; (X Y) (N N (String U Image) X -> (values Image (Y N N -> X U False)))
;; generate "buttons" for simulated hierarchical GUI within big-bang 
(define (button w-start w-end h-start h-end label status)
  (define BACK (overlay
                (rectangle (- w-end w-start) (- h-end h-start) 'outline 'black)
                (rectangle (- w-end w-start) (- h-end h-start) 'solid 'gray)))
  (define BUTT (overlay (if (image? label) label (text label 16 'red)) BACK))
  (define (clicked? x y)
    (if (and (< w-start x w-end) (< h-start y h-end))
        status
        #f))
  (values BUTT clicked?))

;; ---------------------------------------------------------------------------------------------------
;; the song structures we send 

(define SEPARATOR  #"|") ;; one byte that is unlikely to occur in song titles 
(define SONG+TITLE (byte-regexp (bytes-append #"(.*?)\\" SEPARATOR #"(.*)")))
(define GOOD-TITLE (byte-regexp (bytes-append #"\\" SEPARATOR)))

(define GOOD-TITLE-ERROR (format "string that does not contain ~a" SEPARATOR))

(define (song-bytes? s)
  (check-arg 'song-bytes-title (bytes? s) "byte string" "the" s)
  (define m (song-bytes-aux? s))
  (cons? m))

;; Bytes -> #false U (list Bytes Bytes Bytes)
(define (song-bytes-aux? s)
  (regexp-match SONG+TITLE s))
  
(define (make-song-bytes t:str mp3:bytes)
  (check-arg 'make-song-bytes (string? t:str) "string" "first" t:str)
  (check-arg 'make-song-bytes (bytes? mp3:bytes) "byte string" "second" mp3:bytes)
  (define t:byt (string->bytes/utf-8 t:str))
  (check-arg 'make-song-bytes (good-title? t:byt) GOOD-TITLE-ERROR "first" t:byt)
  (bytes-append t:byt SEPARATOR mp3:bytes))

(define (song-bytes-title s)
  (check-arg 'song-bytes-title (bytes? s) "byte string" "the" s)
  (first (song-bytes->both 'song-bytes-title s)))

(define (song-bytes-mp3 s)
  (check-arg 'song-bytes-bytes (bytes? s) "byte string" "the" s)
  (second (song-bytes->both 'song-bytes-mp3 s)))

;; Bytes -> Boolean
(module+ test
  (check-true  (good-title? #"a"))
  (check-false (good-title? #"b|c")))
(define (good-title? t)
  (not (regexp-match GOOD-TITLE t)))

;; Symbol Bytes -> [List String Bytes]
(module+ test (check-equal? (song-bytes->both 'name (make-song-bytes "a" #"b")) (list "a" #"b")))
(module+ test (check-equal? (song-bytes->both 'name (make-song-bytes "a" #"b|c")) (list "a" #"b|c")))
(define (song-bytes->both name t+s)
  (define m (song-bytes-aux? t+s))
  (check-arg name (cons? m) "song-bytes" "the" t+s)
  (match-define (list _all title0 song) m)
  (define title
    (with-handlers ([exn:fail:contract? (lambda (xn) #f)])
      (bytes->string/utf-8 title0)))
  (check-arg name title "song-bytes" "the" t+s)
  (list title song))

(module+ test
  (define short (file-as-bytes "private/short.mp3"))
  (check-exn #px"song-bytes as the argument" (lambda () (song-bytes-title short)))

  (check-equal? (make-song-bytes "a" #"b") #"a|b")
  (check-exn exn:fail? (lambda () (make-song-bytes "a|" #"b")))
  (check-equal? (song-bytes-title (make-song-bytes "a" #"b")) "a")
  (check-equal? (song-bytes-mp3 (make-song-bytes "a" #"b")) #"b"))

;; ---------------------------------------------------------------------------------------------------

(define (self-test name)
  (check-arg name (string? name) "string" "the" name)
  
  (define MP3-short "private/short.mp3")
  (define MP3-long  "private/long.mp3")

  ;; -> Void
  ;; sapawn a music server 
  (define (server-our-job)
    (define msg0 (make-song-bytes "short" (file-as-bytes MP3-short)))
    (define msg1 (make-song-bytes "long"  (file-as-bytes MP3-long)))
    (void
     (universe (cons 1 '())
       [on-msg  (match-lambda*
                  [(list (cons h worlds) from msg)
                   (define mails (map (λ (w) (make-mail w (if (zero? h) msg0 msg1))) worlds))
                   (define kills '())
                   (make-bundle (cons (- 1 h) worlds) mails kills)])]
       [on-new  (match-lambda*
                  [(list (cons h worlds) w)
                   (make-bundle (cons h (cons w worlds)) (list (make-mail w msg0)) '())])])))

  ;; State = False U Response
  
  ;; Any -> State 
  (define (client-their-job _)
    (big-bang #false
      [register   LOCALHOST]
      [to-draw    show]
      [on-receive (λ (w msg) (make-package (play-sound (song-bytes-mp3 msg)) 'next))]))

  ;; State -> Image 
  (define (show w)
    (define msg (string-append name "I am ... waiting... waiting..."))
    (if (boolean? w) (text msg 22 "red") (text w 22 "black")))

  ;; run Lola run 
  (launch-many-worlds  (server-our-job)  (client-their-job 0)))