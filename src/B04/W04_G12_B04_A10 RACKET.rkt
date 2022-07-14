;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |W04_G12_B04_A10 RACKET|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; **************************************************************************
;; **  Hilfsfunktionen
;; **************************************************************************

;; [Hier Hilfsfunktionen spezifizieren, definieren und testen.

(define (calcMySigmaMinus my sigma multiplier)
  (- my (* multiplier sigma)))

(check-expect (calcMySigmaMinus 100 20 3) 40) 

(define (calcMySigmaPlus my sigma multiplier)
  (+ my (* multiplier sigma)))

(check-expect (calcMySigmaPlus 100 20 3) 160)

(define (berechneNote max my sigma punkte)
  (cond
        [(<= punkte (calcMySigmaMinus my sigma 2)) 5] ;; Wenn punkte kleiner als (μ - 2 · σ) gebe 5 aus.
        [(<= punkte (calcMySigmaMinus my sigma 1)) 4] ;; Wenn punkte kleiner als (μ - σ) gebe 4 aus.
        [(<= punkte (calcMySigmaPlus my sigma 1)) 3] ;; Wenn punkte kleiner als (μ + σ) gebe 3 aus.
        [(<= punkte (calcMySigmaPlus my sigma 2)) 2] ;; Wenn punkte kleiner als (μ + 2 · σ) gebe 2 aus.
        [else 1] ;; Ansonsten gebe 1 aus, da Wert über/gleich (μ + 2 · σ) ist.
        ))


;; **************************************************************************
;; **  Schnittstellenfunktion note 
;; **************************************************************************

;; Signatur: note: number number number number -> number

;; Zweck: Gegeben eine maximal erreichbare Punktzahl max, einen Mittelwert my,
;; eine Standardabweichung sigma und eine Punktzahl punkte, bestimmt die
;; gemaess der Aufgabenstellung zu berechnende Note bzw. melde einen Fehler.

;; (define (note max my sigma punkte) ...)

;; Beispiele:
;; - Ergebnisse
;; (note 180 100 20  10)  sollte 5 ergeben
;; (note  90  45 10  31)  sollte 4 ergeben
;; (note 180  45 10  42)  sollte 3 ergeben
;; (note  90  50 10  60)  sollte 3 ergeben
;; (note 180 100 20 121)  sollte 2 ergeben
;; (note 180 100 20 146)  sollte 1 ergeben
;; (note 180  45 10 180)  sollte 1 ergeben
;; - Fehlermeldungen
;; (note  0  5  1  5)  sollte einen Fehler ergeben
;; (note 25 15  0 15)  sollte einen Fehler ergeben
;; (note 20  5 -1  5)  sollte einen Fehler ergeben
;; (note 20 25  1  5)  sollte einen Fehler ergeben
;; (note 30 20  5  5)  sollte einen Fehler ergeben
;; (note 30 20 15 50)  sollte einen Fehler ergeben

;; Definition [Ausgestaltung der Signatur]:
(define (note max my sigma punkte)

  (cond
    [(or (<= max 0) (<= sigma 0)) (error 'note "Ungueltiger Parameter uebergeben")] ;; Prüfen ob max und sigma eine positive Zahl ist
    [(or (> my max) (< my 0)) (error 'note "Ungueltiger Parameter uebergeben")] ;; Prüfen ob my im Intervall [0;max] liegt
    [(or (> punkte max) (< punkte 0))  (error 'note "Ungueltiger Parameter uebergeben")] ;; Prüfe ob punkte im Intervall [0;max] liegt
    [(or (> (calcMySigmaMinus my sigma 3) max) (< (calcMySigmaMinus my sigma 3) 0)) (error 'note "Ungueltiger Parameter uebergeben")] ;; Prüfen ob (μ - 3 · σ) im Intervall [0, max] liegt.
    [(or (> (calcMySigmaPlus my sigma 3) max) (< (calcMySigmaPlus my sigma 3) 0)) (error 'note "Ungueltiger Parameter uebergeben")] ;; Prüfen ob (μ + 3 · σ) im Intervall [0, max] liegt.
    [else (berechneNote max my sigma punkte)])) ;; Führe Berechnung aus, weil alle Bedingungen erfüllt sind.


;; Tests: [Sowohl Fehlerueberpruefung als auch weitergereichte Ergebnisse]
;; - Ergebnisse
(check-expect (note 180 100 20  10)   5) 
(check-expect (note  90  45 10  31)   4) 
(check-expect (note 180  45 10  42)   3) 
(check-expect (note  90  50 10  60)   3) 
(check-expect (note 180 100 20 121)   2)
(check-expect (note 180 100 20 146)   1)
(check-expect (note 180  45 10 180)   1)
;; - Fehlermeldungen
(check-error (note  0  5  1  5) "note: Ungueltiger Parameter uebergeben")
(check-error (note 25 15  0 15) "note: Ungueltiger Parameter uebergeben")
(check-error (note 20  5 -1  5) "note: Ungueltiger Parameter uebergeben") 
(check-error (note 20 25  1  5) "note: Ungueltiger Parameter uebergeben") 
(check-error (note 30 20  5  5) "note: Ungueltiger Parameter uebergeben") 
(check-error (note 30 20 15 50) "note: Ungueltiger Parameter uebergeben") 
