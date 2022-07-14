;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |W04_G12_B04_A12 RACKET|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; **************************************************************************
;; **  Hilfsfunktionen 
;; **************************************************************************

;; [Hier Hilfsfunktionen spezifizieren, implementieren, testen]

;; Errechnen des Betrags der alten Mehrwertsteuer aus dem alten Preis (price) und der alten Mehrwertsteuer in % (old-tax)
(define (oldTaxAmount price old-tax)
  (* (/ price (+ 100 old-tax)) old-tax))

(check-expect (oldTaxAmount 100 20) (/ 50 3))



;; Errechnung des Grundpreises aus dem alten Preis (price) und der alten Mehrwertsteuer in % (old-tax)
(define (basicPrice price old-tax)
  (* (/ price (+ old-tax 100)) 100))

(check-expect (basicPrice 100 20) (/ 250 3))



;; Errechnung des Betrags der neuen Mehrwersteuer aus dem alten Preis (price), der alten Mehrwertsteuer in %(old-tax) und der neuen Mehrwertsteuer in %(new-tax)
(define (newTaxAmount price old-tax new-tax)
  (* (/ (basicPrice price old-tax) 100) new-tax))

(check-expect (newTaxAmount 100 20 15) 12.5)



;; Berechnung des neuen Preises durch den alten Preis (price) und der Beträge der vorher erechneten alten und neuen Mehrwertsteuer
(define (newPrice price old-tax new-tax)
  (+(- price (oldTaxAmount price old-tax)) (newTaxAmount price old-tax new-tax)))

(check-expect (newPrice 100 20 15) (/ 575 6))



;; Schneidet alles nach der zweiten Nachkommastelle ab und verschiebt das Komma eins nach rechts (z.B. 12,2145 => 122,1).
(define (cutDecimal price old-tax new-tax) 
  (/ (floor (* (newPrice price old-tax new-tax) 100)) 10))

(check-expect (cutDecimal 100 20 15) 958.3)



;; Ermittelt den Wert der letzten Dezimalzahl nach dem Komma (Bedingung: Nur eine Nachkommastelle).
(define (cutBeforeComma price old-tax new-tax) 
  (* 10 (- (cutDecimal price old-tax new-tax) (floor (cutDecimal price old-tax new-tax)))))

(check-expect (cutBeforeComma 100 20 15) 3)



;; Rundet den neu erechneten Preis ab
(define (roundDown price old-tax new-tax)
  (/ (floor (* (newPrice price old-tax new-tax) 10)) 10))

(check-expect (roundDown 100 20 15) 95.8)



;; Rundet den neu erechneten Preis auf
(define (roundUp price old-tax new-tax)
  (/ (ceiling (* (newPrice price old-tax new-tax) 10)) 10))

(check-expect (roundUp 100 20 15) 95.9)

;; **************************************************************************
;; **  Funktion change-price 
;; **************************************************************************

;; Signatur: change-price: number number number -> number

;; Zweck: Gegeben eine Zahl price, eine alte Mehrwertsteuer old-tax und eine
;; neue Mehrwertsteuer new-tax (jeweils in Prozent), bestimme den exakten Preis
;; nach der Aenderung der Mehrwertsteuer von old-tax zu new-tax wobei 
;; gemaess der in Aufgabe 12 gegebenen Regeln auf Betraege in Einheiten
;; zu fuenf Cent gerechnet wird.

;; (define (change-price price old-tax new-tax) ...)

;; Beispiele:
;; (change-price  0.00 10.5 12.3)   sollte   0.00 ergeben. [Keine Rundung   ]
;; (change-price 17.45 10.5 10.5)   sollte  17.45 ergeben. [Keine Rundung   ]
;; (change-price 11.90 19   16  )   sollte  11.60 ergeben. [Keine Rundung   ]
;; (change-price 15.00 19   16  )   sollte  14.60 ergeben. [Abrunden  auf  0]
;; (change-price 10.24  7    9  )   sollte  10.45 ergeben. [Aufrunden auf  5]
;; (change-price  9.43  9    7  )   sollte   9.25 ergeben. [Keine Rundung   ]
;; (change-price 16.05 12.5 10.5)   sollte  15.75 ergeben. [Abrunden  auf  5]
;; (change-price 17.25 16   19  )   sollte  17.70 ergeben. [Aufrunden auf 10]

;; Definition [Ausgestaltung der Signatur]:
(define (change-price price old-tax new-tax)
   (cond [(= price 0) price]
     [ (and (<= (cutBeforeComma price old-tax new-tax) 2) (> (cutBeforeComma price old-tax new-tax) 0)) (roundDown price old-tax new-tax) ]
       [ (and (< (cutBeforeComma price old-tax new-tax) 8) (> (cutBeforeComma price old-tax new-tax) 2)) (+ 0.05 (roundDown price old-tax new-tax)) ]
        [ (and (<= (cutBeforeComma price old-tax new-tax) 9) (>= (cutBeforeComma price old-tax new-tax) 8)) (roundUp price old-tax new-tax) ]
         [ else (newPrice price old-tax new-tax)] ))

;; Tests:
(check-expect (change-price  0.00 10.5 12.3)   0.00)
(check-expect (change-price 17.45 10.5 10.5)  17.45)
(check-expect (change-price 11.90 19   16  )  11.60)
(check-expect (change-price 15.00 19   16  )  14.60)
(check-expect (change-price 10.24  7    9  )  10.45)
(check-expect (change-price  9.43  9    7  )   9.25)
(check-expect (change-price 16.05 12.5 10.5)  15.75)
(check-expect (change-price 17.25 16   19  )  17.70)