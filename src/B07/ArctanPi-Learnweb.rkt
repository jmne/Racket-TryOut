;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ArctanPi-Learnweb) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; Signatur: reihe : lambda number -> number
;; Zweck: Berechnet die Reihe a(0) + a(1) + a(2) + ...
;;    solange bis |a(i)| < eps. Dabei ist a ein Lambda-Ausdruck,
;;    der eine Zahl entgegennimmt und eine Zahl zurueckgibt.
;; Beispiele:
;;  (reihe (lambda (n) (expt 1/2 n)) 1e-6) sollte etwa 2 ergeben.
;;  (reihe (lambda (n) (expt 1/10 n)) 1e-8) sollte etwa 10/9 ergeben.
;;  (reihe (lambda (n) (/ 1 (* (+ n 1) (+ n 1)))) 1e-8) sollte etwa pi^2/6 ergeben.
;; Definition:
(define (reihe a eps)
  (calc a eps 0 0)) ;; Rufe Berechnung für Folge auf

;; Tests:
(check-within (reihe (lambda (n) (expt 1/2 n)) 1e-6) 2 1e-3)
(check-within (reihe (lambda (n) (expt 1/10 n)) 1e-8) 10/9 1e-5)
(check-within (reihe (lambda (n) (/ 1 (* (+ n 1) (+ n 1)))) 1e-8) (/ (* pi pi) 6) 1e-4)


  (define (calc a eps i summe)

    (if (>= (abs(a i)) eps) ;; Wenn a(i) >= eps ist,
        (calc a eps (+ i 1) (+ summe (a i))) ;; rufe Funktion rekursiv auf mit a(i+1) und addiere zu Summe
        summe) ;; Ansonsten gib summe aus
    )


;; Signatur: approx-pi : number -> number
;; Zweck: Berechnet einen Naehrungswert fuer pi mittels Machin-Formel.
;; Beispiele: (approx-pi 1e-3) sollte 3.140596... ergeben
;;            (approx-pi 1e-7) sollte 3.1415916745... ergeben
;;            (approx-pi 1e-10) sollte 3.1415926526163... ergeben
;; Definition:
(define (approx-pi eps)

  (* 4
     (-
      (* 4 (reihe arctan1/5 eps))
      (reihe arctan1/239 eps))) ;; Approximiere Pi mit der Formel pi/4 = 4 * arctan(1/5) - arctan(1/239)
)

;; Tests:

(check-within (approx-pi 1e-3) pi 1e-2)
(check-within (approx-pi 1e-7) pi 1e-6)
(check-within (approx-pi 1e-10) pi 1e-9)


;; Hilfsfunktion


;; Signatur: arctan1/5 : number -> number
;; Zweck: Berechnen von Arctan(1/239)
;; Beispiel: (arctan1/5 0) sollte 0.2 ergeben
;; Definition
(define arctan1/5
  (lambda (n) (* (expt -1 n) (/ (expt 1/5 (+ (* 2 n) 1))(+ (* 2 n) 1))))) ;; Formel arctan(1/5)

(check-within (arctan1/5 0) 0.2 1e-2)


;; Signatur: arctan1/239 : number -> number
;; Zweck: Berechnen von Arctan(1/239)
;; Beispiel: (arctan1/239 0) sollte 0.0041.. ergeben
;; Definition
(define arctan1/239
  (lambda (n) (* (expt -1 n) (/ (expt 1/239 (+ (* 2 n) 1))(+ (* 2 n) 1))))) ;; Formel arctan(1/239)

(check-within (arctan1/239 0) 0.0041841 1e-2)