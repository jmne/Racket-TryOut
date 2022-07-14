;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname BonuspunkteDispatch-Learnweb) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ---------------------------------------------------------------------
;; Vorlage für Aufgabe 15
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

;; Kommission als multiplikativer Faktor
(define commission-factor 0.10)

;; ---------------------------------------------------------------------

;; Hier Datendefinitionen vornehmen.
;; Insbesondere muss erklaert werden, was der Datentyp bonuspunkte ist,
;; der in der Funktion convert (s.u.) genutzt wird.

;; Der Datentyp bonuspunkte besteht den Komponenten punkte (Zahl), extra (beliebig) und firma (Buchstabe).

;; Datendefinition
(define-struct bonuspunkte (punkte extra firma))
;; bonuspunkte ist eine Struktur (make-bonuspunkte punkte extra firma).

;; ---------------------------------------------------------------------
;; Hilfsfunktionen
;; ---------------------------------------------------------------------

(define (isCorrectSymbol symbol) ;; Prüft ob die richtige Conversion eingegeben wurde
(cond [(symbol=? symbol '->A) false]
      [(symbol=? symbol '->B) false]
      [(symbol=? symbol '->C) false]
      [else true]))

(check-expect (isCorrectSymbol '->B) false)
(check-expect (isCorrectSymbol '->D) true)


;; ---------------------------------------------------------------------

(define (getConversionFactor conversion a-bonuspunkte) ;; Gibt zurück, welcher Umrechnungfaktor benutzt werden soll.
  (cond [(and (symbol=? conversion '->B) (symbol=? (bonuspunkte-firma a-bonuspunkte) 'A)) b-per-a]
        [(and (symbol=? conversion '->A) (symbol=? (bonuspunkte-firma a-bonuspunkte) 'B)) a-per-b]
        [(and (symbol=? conversion '->C) (symbol=? (bonuspunkte-firma a-bonuspunkte) 'B)) c-per-b]
        [(and (symbol=? conversion '->B) (symbol=? (bonuspunkte-firma a-bonuspunkte) 'C)) b-per-c]
        ))

;;(check-expect (getConversionFactor '->B) 1.05)

;; ---------------------------------------------------------------------

(define (calcNewAmount conversionFactor amount a-bonuspunkte) ;; Berechnet die Anzahl der neuen Bonuspunkte.
  (make-bonuspunkte (floor(* (* conversionFactor amount a-bonuspunkte) 0.9))) )

;;(check-expect (calcNewAmount 10.5 10)  94)


;; ---------------------------------------------------------------------

(define (calcDoubleAmountAC amount a-bonuspunkte) ;; Berechnet die Anzahl der neuen Bonuspunkte.
  (calcNewAmount (getConversionFactor '->B a-bonuspunkte) (calcNewAmount (getConversionFactor '->C a-bonuspunkte) amount a-bonuspunkte) a-bonuspunkte))

;;(check-expect (calcNewAmount 10.5 10)  94)


;; ---------------------------------------------------------------------

(define (calcDoubleAmountCA amount a-bonuspunkte) ;; Berechnet die Anzahl der neuen Bonuspunkte.
  (calcNewAmount (getConversionFactor '->B a-bonuspunkte) (calcNewAmount (getConversionFactor '->A a-bonuspunkte) amount a-bonuspunkte) a-bonuspunkte))

;;(check-expect (calcNewAmount 10.5 10)  94)


;; ---------------------------------------------------------------------

(define (checkSameOrigin direction a-bonuspunkte)
  (cond [(and (symbol=? direction '->A) (symbol=? a-bonuspunkte beispiel-a)) true] ;;Vergleichen?
        [(and (symbol=? direction '->B) (symbol=? a-bonuspunkte beispiel-b)) true]
        [(and (symbol=? direction '->C) (symbol=? a-bonuspunkte beispiel-c)) true]
        [else false]))

;; ---------------------------------------------------------------------
;; Hauptfunktion
;; ---------------------------------------------------------------------

;; Signatur: convert : bonuspunkte symbol -> bonuspunkte
;; Zweck: Wandelt die angegebenen Bonuspunkte in die angegebene
;; Konvertierungsrichtung um.

;; Schablone:
;; [hier Schablone ergaenzen

;; Datenbuendel fuer Tests:
;; [200 Bonuspunkte fuer AllesLieferbar mit 10 Premium-Bestellungen]
(define beispiel-a (make-bonuspunkte 200 10 'A) )
;; [ 50 Bonuspunkte fuer BestellMa(h)l mit 3 Gratis-Desserts]
(define beispiel-b (make-bonuspunkte 50 3 'B) )
;; [100 Bonuspunkte fuer CaterFruehstueck mit Klingel-Erlaubnis]
(define beispiel-c (make-bonuspunkte 100 true 'C) )

;; Beispiele:
;;
;; [Hier zu erwartende Ergebnisse ergaenzen.]
;;
;; (convert beispiel-a '->A) sollte ... ergeben
;; (convert beispiel-a '->B) sollte ... ergeben
;; (convert beispiel-a '->C) sollte ... ergeben
;;
;; (convert beispiel-b '->A) sollte ... ergeben
;; (convert beispiel-b '->B) sollte ... ergeben
;; (convert beispiel-b '->C) sollte ... ergeben
;;
;; (convert beispiel-c '->A) sollte ... ergeben
;; (convert beispiel-c '->B) sollte ... ergeben
;; (convert beispiel-c '->C) sollte ... ergeben
;;

;; Definition:
(define (convert a-bonuspunkte direction)
  (cond [(isCorrectSymbol direction) (error 'convert "Ungueltige Konvertierungsrichtung uebergeben.")]
        [(not (bonuspunkte? a-bonuspunkte)) (error 'convert "Parameter vom Typ bonuspunkte erwartet.")]
        [(checkSameOrigin direction a-bonuspunkte) a-bonuspunkte]
        [(and (symbol=? (bonuspunkte-firma a-bonuspunkte) 'A) (symbol=? direction 'C)) (calcDoubleAmountAC (bonuspunkte-punkte a-bonuspunkte) a-bonuspunkte)]
        [else (calcNewAmount (getConversionFactor direction a-bonuspunkte) (bonuspunkte-punkte a-bonuspunkte) a-bonuspunkte)])  ;; Gibt die neue Anzahl an Bonuspunkten aus
        )

;; Tests
(check-expect (convert beispiel-a '->A) ... )
(check-expect (convert beispiel-a '->B) ... )
(check-expect (convert beispiel-a '->C) ... )
(check-expect (convert beispiel-b '->A) ... )
(check-expect (convert beispiel-b '->B) ... )
(check-expect (convert beispiel-b '->C) ... )
(check-expect (convert beispiel-c '->A) ... )
(check-expect (convert beispiel-c '->B) ... )
(check-expect (convert beispiel-c '->C) ... )
(check-error  (convert 'Beispiel  '->C) "convert: Parameter vom Typ bonuspunkte erwartet.")
(check-error  (convert beispiel-c '->D) "convert: Ungueltige Konvertierungsrichtung uebergeben.")
