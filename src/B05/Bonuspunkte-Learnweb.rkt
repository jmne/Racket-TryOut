;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Bonuspunkte-Learnweb) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ---------------------------------------------------------------------

;; B-Bonuspunkte pro A-Bonuspunkt
(define b-per-a 1.05)

;; A-Bonuspunkte pro B-Bonuspunkt
(define a-per-b 0.95)

;; B-Bonuspunkte pro C-Bonuspunkt
(define b-per-c 10.5)

;; C-Bonuspunkte pro B-Bonuspunkt
(define c-per-b 0.09)

;; **************************************************************************
;; **  Weitere Faktoren sollen nicht explizit definiert werden.
;; **  Aus ethischer Sicht ist dies nicht korrekt, aus Sicht der Gewinn-
;; **  maximierung und aus didaktischen Gruenden hier(!) schon.
;; **************************************************************************

;; Hier ggfs. Hilfsfunktionen spezifizieren, implementieren und testen.

(define (isCorrectSymbol symbol) ;; Prüft ob die richtige Conversion eingegeben wurde
(cond [(symbol=? symbol 'A->B) false]
      [(symbol=? symbol 'A->C) false]
      [(symbol=? symbol 'B->A) false]
      [(symbol=? symbol 'B->C) false]
      [(symbol=? symbol 'C->A) false]
      [(symbol=? symbol 'C->B) false]
      [else true]))

(check-expect (isCorrectSymbol 'A->B) false)
(check-expect (isCorrectSymbol 'A->A) true)

;; ---------------------------------------------------------------------

(define (getConversionFactor conversion) ;; Gibt zurück, welcher Umrechnungfaktor benutzt werden soll.
  (cond [(symbol=? conversion 'A->B) b-per-a]
        [(symbol=? conversion 'B->A) a-per-b]
        [(symbol=? conversion 'B->C) c-per-b]
        [(symbol=? conversion 'C->B) b-per-c]))

(check-expect (getConversionFactor 'A->B) 1.05)

;; ---------------------------------------------------------------------

(define (calcDoubleStepAC amount) ;; Führt eine doppelte Berechnung durch für A->C (A->B B->C)
  (calcNewAmount (getConversionFactor 'B->C) (calcNewAmount (getConversionFactor 'A->B) amount)))

(check-expect (calcDoubleStepAC 20) 1)

;; ---------------------------------------------------------------------

(define (calcDoubleStepCA amount) ;; Führt eine doppelte Berechnung durch für C->A (C->B B->A)
  (calcNewAmount (getConversionFactor 'B->A) (calcNewAmount (getConversionFactor 'C->B) amount)))

(check-expect (calcDoubleStepCA 30) 241)

;; ---------------------------------------------------------------------

(define (calcNewAmount conversionFactor amount) ;; Berechnet die Anzahl der neuen Bonuspunkte.
  (floor(* (* conversionFactor amount) 0.9)))

(check-expect (calcNewAmount 10.5 10)  94)


;; ---------------------------------------------------------------------

;; Signatur: convert-bonus-points : symbol number -> number

;; Zweck: Umrechnung der angegebenen Bonuspunkte in der
;; angegebenen Richtung. 

;; Beispiele: 
;; (convert-bonus-points 'C->B 10) sollte  94 ergeben
;; (convert-bonus-points 'A->B 20) sollte  18 ergeben
;; (convert-bonus-points 'C->A 30) sollte 241 ergeben
;; (convert-bonus-points 'A->C 20) sollte   1 ergeben
;; (convert-bonus-points 'B->A 50) sollte  42 ergeben
;; (convert-bonus-points 'B->C 90) sollte   7 ergeben

;; Definition: 
(define (convert-bonus-points conversion amount)
  (cond [(>= 0 amount) "Falscher Parameter 'amount' übergeben"]  ;; Testet ob ein richtiger Parameter übergeben wurde
        [(isCorrectSymbol conversion) "Falscher Parameter 'conversion' übergeben"] ;; ""
        [(symbol=? conversion 'A->C) (calcDoubleStepAC amount)] ;; Gibt die neue Anzahl an Bonuspunkten aus für den Spezialfall A->C
        [(symbol=? conversion 'C->A) (calcDoubleStepCA amount)] ;; Gibt die neue Anzahl an Bonuspunkten aus für den Spezialfall C->A
        [else (calcNewAmount (getConversionFactor conversion) amount)])  ;; Gibt die neue Anzahl an Bonuspunkten aus
  )

;; Tests
(check-expect (convert-bonus-points 'C->B 10)  94)
(check-expect (convert-bonus-points 'A->B 20)  18)
(check-expect (convert-bonus-points 'C->A 30) 241)
(check-expect (convert-bonus-points 'A->C 20)   1)
(check-expect (convert-bonus-points 'B->A 50)  42)
(check-expect (convert-bonus-points 'B->C 90)   7)
