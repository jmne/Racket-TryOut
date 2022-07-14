;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Aufgabe19) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ---------------------------------------------------------------------
;; Aufgabenteil (a)
;; ---------------------------------------------------------------------

;; Signatur: nachfolger : list -> list
;; Zweck: Berechnet Nachfolgertupel des als Liste übergebenen Tupels t.
;; Beispiele:
;;    (nachfolger (list 2 6 7 11)) sollte (list 4 1 4 9) ergeben.
;;    (nachfolger empty) sollte empty ergeben.
;;    (nachfolger (list 42)) sollte (list 0) ergeben.
;;    (nachfolger (list 13 37 42)) sollte (list 24 5 29) ergeben.
;;    (nachfolger (list 0 0 0 0 0)) sollte (list 0 0 0 0 0) ergeben.
;; Schablone:
;;   Singatur: process-list : a-list -> ???
;;          (define (process-list a-list)
;;              (if
;;                 (and (not (empty? a-list)) (list? a-list)) (... a-list empty (first a-list))
;;                  empty))
;; Definition der abgesichterten Hauptfunktion
(define (nachfolger t) 
 (if
   (and ( not ( empty? t))(list? t)) (processList t empty (first t))
   empty))
  

;; Tests:
(check-expect (nachfolger (list 2 6 7 11)) (list 4 1 4 9))
(check-expect (nachfolger empty)           empty)
(check-expect (nachfolger (list 42))       (list 0))
(check-expect (nachfolger (list 13 37 42)) (list 24 5 29))
(check-expect (nachfolger (list 0 0 0 0))  (list 0 0 0 0))


;; Hier Hilfsfunktionen definieren.
;; Signatur: processList: list list number -> list
;; Zweck: Errechnet rekusiv die Elemente des Nachfolgetupels
;; Schablone:
;;   Singatur: a-list a-list a-number -> ???
;;          (define (processList a-list a-list a-number)
;;              (if
;;                 (empty? a-list)) (...(...(... ... a-number) a-list))
;;                  (... a-list(...(...(... a-number a-number)) a-list)...)
;; Definition der nicht abgesicherten Hilfsfunktion
(define (processList t p firstElement)
  (if
    (empty? (rest t)) (reverse (cons (lastElement firstElement (first t)) p))
    (processList (rest t) (cons (abs (- (first t) (first (rest t)))) p ) firstElement)))


;; Tests:
(check-expect (processList (list 2 6 7 11) empty 2) (list 4 1 4 9))
(check-expect (processList (list 42) empty 42) (list 0))

;; Signatur: lastElement: a-number a-number -> a-number
;; Zweck: Erechnet das letzte Element des Nachfolgetupels
(define (lastElement t p)
  (abs (- p t)))

;; Tests:
(check-expect (lastElement 2 11) 9)
;; ---------------------------------------------------------------------
;; Aufgabenteil (b)
;; ---------------------------------------------------------------------

;; Signatur: nachfolgerstapel : list -> list
;; Zweck: Erzeugt Liste der Nachfolgertupel eines Tupels t.
;; Beispiele:
;;    (nachfolgerstapel (list 2 6 7 11)) sollte
;;      (list (list 2 6 7 11) (list 4 1 4 9) (list 3 3 5 5)
;;            (list 0 2 0  2) (list 2 2 2 2) (list 0 0 0 0)) ergeben.
;;    (nachfolgerstapel (list 0)) sollte (list (list 0)) ergeben.
;;    (nachfolgerstapel (list 23 41 11 18)) sollte
;;      (list (list 23 41 11 18) (list 18 30  7  5) (list 12 23 2 13)
;;            (list 11 21 11  1) (list 10 10 10 10) (list  0  0 0  0)) ergeben.
;;    (nachfolgerstapel empty) sollte einen Fehler erzeugen.
;;    (nachfolgerstapel (list 1 1 1)) sollte einen Fehler erzeugen.
;; Schablone: 
;;   Signatur: process-list: a-list-> ???
;;          (define (process-list a-list)
;;              (cond
;;                 [ (empty? a-list) (error ...)]
;;                 [ (not (... (length a-list))) (error ...)]
;;                 [ else (... a-list empty)]
;;
;; Definition der abgesicherten Hauptfunktion
(define (nachfolgerstapel t)
  (cond
   [(empty? t) (error 'nachfolgerstapel "Tupel ungueltiger Laenge uebergeben.")]
   [(not (potencyOfTwo (length t))) (error 'nachfolgerstapel "Tupel ungueltiger Laenge uebergeben.")]
   [else (processListSteps t empty)]))
   


  
;; Tests:
(check-expect (nachfolgerstapel (list 2 6 7 11))
   (list (list 2 6 7 11) (list 4 1 4 9) (list 3 3 5 5)
         (list 0 2 0  2) (list 2 2 2 2) (list 0 0 0 0)))
(check-expect (nachfolgerstapel (list 0))
   (list (list 0)))
(check-expect (nachfolgerstapel (list 23 41 11 18))
   (list (list 23 41 11 18) (list 18 30  7  5) (list 12 23 2 13)
         (list 11 21 11  1) (list 10 10 10 10) (list  0  0 0  0)))
(check-error (nachfolgerstapel empty) "nachfolgerstapel: Tupel ungueltiger Laenge uebergeben.")
(check-error (nachfolgerstapel (list 1 1 1)) "nachfolgerstapel: Tupel ungueltiger Laenge uebergeben.")

;; Hier Hilfsfunktionen definieren.

;; Signatur: processListSteps: list-of-numbers list-of-numbers-> list-of-numbers
;; Zweck: Errechnet rekusiv die Elemente der Nachfolgetupel
;; Schablone:
;;   Singatur: process-list : a-list a-list -> ???
;;          (define (process-list a-list a-list )
;;              (cond
;;                 [(... a-list a-number)) (...(... a-list a-list)]
;;                 [(else (... (... a-list a-list a-number) (... a-list a-list)))]
;; Definition der nicht abgesicherten Hilfsfunktion
(define (processListSteps t p)
  
  (cond
    [ (isListZero t 0) (reverse (cons t p)) ]
    [ else  (processListSteps (processList t empty (first t)) (cons t p)) ]))

;;Tests:
(check-expect (processListSteps (list 2 6 7 11) empty) (list (list 2 6 7 11)
(list 4 1 4 9) (list 3 3 5 5) (list 0 2 0 2) (list 2 2 2 2) (list 0 0 0 0)))

(check-expect (processListSteps (list 1 8 7 15) empty) (list (list 1 8 7 15)
(list 7 1 8 14) (list 6 7 6 7) (list 1 1 1 1) (list 0 0 0 0)))


;; Signatur: potencyOfTwo : a-number-> boolean
;; Zweck: Prüft ob die Liste die Länge einer zweier Potenz hat
;; Definition der nicht abgesicherten Hilfsfunktion

(define (potencyOfTwo t)
  (cond
   [(= 1 t) true]
   [(> 1 t) false]
      [else (potencyOfTwo (/ t 2))]))


;;Tests:
(check-expect (potencyOfTwo 5) false)
(check-expect (potencyOfTwo 8) true)  

;; Signatur: isListZero: list number -> boolean
;; Zweck:  Prüft ob das Nulltupel erreicht ist
;; Schablone:
;;   Singatur: process-list : a-list a-number -> ???
;;          (define (process-list a-list a-number)
;;              (cond
;;                 [(> a-number 0)) false]
;;                 [(empty? a-list) true]
;;                 [else (... (rest a-list) (+ (first a-list) a-number))]
;; Definition der nicht abgesicherten Hilfsfunktion

(define (isListZero t sum)
  (cond
   [ (> sum 0) false]
   [(empty? t) true]
   [else (isListZero (rest t) (+ (first t) sum))]))
;;Tests:
(check-expect (isListZero (list 1) 0) false)
(check-expect (isListZero (list 0 0 0) 0) true)