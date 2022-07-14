;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Blatt03Aufgabe08mitCheckWithin) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Signatur: Bremsweg : number number number-> number 
;; Zweck: Berechnung des Bremsweg in Abhängigkeit von Bremsbeschleunigung und Geschwindikeit und Reaktionszeit

;; Umrechnung von der Eingabe Geschwindigkeit in km/h zu Geschwindigkeit in m/s
(define (MeterProSekunde v)
  (/ v 3.6))
;; Beispiel: (MeterProSekunde 80) sollte 200/9 ergeben. Es werden 80 km/h in 200/9 m/s umgerechnet.
;; Test:
(check-expect (MeterProSekunde 80) (/ 200 9))
(check-expect (MeterProSekunde 10) (/ 25 9))


;; Reaktionsweg in Abhängigkeit von Gefahrene MeterProSekunde und Reaktionszeit in Sekunden(s)
(define (Reaktionsweg v s)
  (* (MeterProSekunde v) s))
;;Beispiel: (Reaktionsweg 80 1) sollte 200/9 ergeben. Es wird der Reaktionsweg bei 80 km/h (200/9 m/s) und einer Sekunde Reaktionszeit errechnet. 
;;Test
(check-expect (Reaktionsweg 80 1 )(/ 200 9))

;;Bremsweg in Abhängikeit von Geschwindigkeit (v) und Bremsbeschleunigung (a)
(define (Bremsweg v a)
  (/ (* (MeterProSekunde v) (MeterProSekunde v)) (* 2 a)))
;;Beispiel: (Bremsweg 80 7) sollte 2000/567 ergeben. Es wird der Bremsweg bei 80 km/h (200/9 m/s) und einer Bremsbeschleunigung von 7 m/s errechnet.
;;Test:
(check-expect (Bremsweg 80 7) (/ 20000 567))
(check-expect (Bremsweg 10 4) (/ 625 648))

;; Hauptfunktion:
;; Anhaltestrecke in Abhängigkeit von Geschwindigkeit (v), Reaktionszeit(s) und Bremsbeschleunigung(a)
(define (Anhaltestrecke v s a)
  (+ (Bremsweg v a) (Reaktionsweg v s)))

;; Beispiel: (Anhaltestrecke 25 1 4) sollte 12.97260802 ergeben. Das Beispiel beschreibt eine Geschwindigkeit von 25 km/h, eine Reaktionszeit von 1 Sekunde und eine Bremsbeschleunigung von 4 m/s.
;;Test:
(check-expect (Anhaltestrecke 80 2 4) (/ 8600 81))
(check-within (Anhaltestrecke 10 4 7) 11.6 0.1)     ;;Selbstnachgerechnet Ergebniss richtig (11.6622575), Programm gibt Ergebniss mit Periode aus
(check-within (Anhaltestrecke 25 1 4) 12.9 0.1)    ;;Selbstnachgerechnet Ergebniss richtig (12.97260802), Programm gibt Ergebniss mit Periode aus
 