;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname DeepReduce-Learnweb) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; ---------------------------------------------------------------------
;; Aufgabenteil (a)
;; ---------------------------------------------------------------------

;; Signatur: pure? : list -> bool
;; Zweck: Prueft, ob die uebergebene Liste ls keine Listen enthaelt.
;; Beispiele:
;;    (pure? empty)                 sollte  true ergeben.
;;    (pure? (list 1 'b 3 "d"))     sollte  true ergeben.
;;    (pure? (list 1 2 3 empty))    sollte false ergeben.
;;    (pure? (list (list 1 2 3 4))) sollte false ergeben.
;; Schablone:
;;    Signatur: process-list : a-list -> ???
;;    (define (process-list a-list)
;;     (cond
;;       [ (empty? a-list) ... ]
;;       [ (list? (first a-list)) ... ]
;;       [ else (process-list (rest a-list)) ]
;;     )
;;    )
;;
;; Definition:
(define (pure? ls)
  (cond
        [(empty? ls) true] ;; Wenn Liste leer gib true zurück
        [(list? (first ls)) false] ;;Wenn erste Element = Liste gib false aus
        [else (pure? (rest ls))] )) ;; Rekursionsaufruf

;; Tests:
(check-expect (pure? empty) true)
(check-expect (pure? (list 1 'b 3 "d")) true)
(check-expect (pure? (list 1 2 3 empty)) false)
(check-expect (pure? (list (list 1 2 3 4))) false)



;; ---------------------------------------------------------------------
;; Aufgabenteil (b)
;; ---------------------------------------------------------------------

;; Signatur: deep-reduce : list -> number
;; Zweck: Aggregiert eine verschachtelte Liste ls, die nur Zahlen enthaelt,
;;    mittels der Aggregatfunktion fun.
;; Beispiele:
;;    (deep-reduce + 42 empty) sollte 42 ergeben.
;;    (deep-reduce + 0 (list 1 2 3 4)) sollte 10 ergeben.
;;    (deep-reduce + 0 (list 1 2 (list 3 (list 4 5) 6) 7 (list (list 8)) empty 9)) sollte 45 ergeben.
;;    (deep-reduce * 1 (list (list 1 2 3 4))) sollte 24 ergeben.
;; Schablone:
;;     Signatur: process-list : a-function a-number a-list -> ???
;; (define (process-list a-function a-number a-list)
;;  (cond
;;        [(empty? a-list) a-number]
;;        [(empty? (first a-list)) (process-list a-function a-number (rest a-list)) ]
;;        [(list? (first a-list)) (process-list a-function (process-list a-function a-number (first a-list)) (rest a-list)) ]
;;        [else (process-list a-function (fun a-number (first a-list)) (rest a-list))]
;;  )
;; )
;;
;; Definition:

(define (deep-reduce fun neutral ls)
  (cond
        [(empty? ls) neutral] ;; Ist Liste leer gib neutral aus
        [(empty? (first ls)) (deep-reduce fun neutral (rest ls)) ] ;; Ist Element der Liste empty führe deep-reduce mit Rest aus
        [(list? (first ls)) (deep-reduce fun (deep-reduce fun neutral (first ls)) (rest ls)) ] ;; Ist Element der Liste "Liste"
;;                    führe deep-reduce zweimal aus. Einmal für den Rest der übergeordneten Liste und einmal für Element "Liste".
        
        [else (deep-reduce fun (fun neutral (first ls)) (rest ls))] ;; Führe deep-reduce aus und berechne erstes Element zu neutral dazu.
  )
)

;; Tests:
(check-expect (deep-reduce + 42 empty) 42)
(check-expect (deep-reduce + 0 (list 1 2 3 4)) 10)
(check-expect (deep-reduce + 0 (list 1 2 (list 3 (list 4 5) 6) 7 (list (list 8)) empty 9)) 45)
(check-expect (deep-reduce * 1 (list (list 1 2 3 4))) 24)