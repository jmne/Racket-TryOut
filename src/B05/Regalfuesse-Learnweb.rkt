;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Regalfuesse-Learnweb) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ---------------------------------------------------------------------
;; Konstanten
;; ---------------------------------------------------------------------
(define PI 3.142)

;; ---------------------------------------------------------------------
;; Aufgabenteil (a)
;; ---------------------------------------------------------------------

;; Datendefinition [ergaenzen]
;; Ein regalfuss ist ein Datenbündel der Strukturen quaderfuss, zylinderfuss oder kegelfuss

;; Datendefinition [ergaenzen]
(define-struct quaderfuss (breite hoehe tiefe))
;; Ein quaderfuss ist eine Struktur bestehend aus Breite, Höhe und Tiefe

;; Datendefinition [ergaenzen]
(define-struct zylinderfuss (durchmesser hoehe))
;; Ein zylinderfuss ist eine Struktur bestehend aus Durchmesser und Höhe

;; Datendefinition [ergaenzen]
(define-struct kegelfuss (durchmesser hoehe))
;; Ein kegelfuss ist eine Struktur bestehen aus Durchmesser und Höhe


;; ---------------------------------------------------------------------
;; Beispiel-Datenbuendel fuer Tests
;; ---------------------------------------------------------------------

;; Ergaenzen Sie die nachfolgenden Definitionen der Beispieldaten
;; in Abhaengigkeit der Definitionen Ihrer Strukturen. Diese werden
;; fuer die vorgegebenen Tests verwendet.

;; qf1 ist ein Quaderfuss mit Breite  4, Hoehe 10 und Tiefe  4.
(define qf1 (make-quaderfuss   4 10 4))
;; qf2 ist ein Quaderfuss mit Breite 13, Hoehe  5 und Tiefe 11.
(define qf2 (make-quaderfuss   13 5 11))
;; qf3 ist ein Quaderfuss mit Breite  8, Hoehe  8 und Tiefe  8.
(define qf3 (make-quaderfuss   8 8 8))

;; zf1 ist ein Zylinderfuss mit Durchmesser  8 und Hoehe 15.
(define zf1 (make-zylinderfuss 8 15))
;; zf2 ist ein Zylinderfuss mit Durchmesser 12 und Hoehe 11.
(define zf2 (make-zylinderfuss 12 11))
;; zf3 ist ein Zylinderfuss mit Durchmesser 10 und Hoehe 10.
(define zf3 (make-zylinderfuss 10 10))

;; kf1 ist ein Kegelfuss mit Durchmesser 12 und Hoehe  7.
(define kf1 (make-kegelfuss 12 7))
;; kf2 ist ein Kegelfuss mit Durchmesser  9 und Hoehe 13.
(define kf2 (make-kegelfuss 9 13))
;; kf3 ist ein Kegelfuss mit Durchmesser  6 und Hoehe  6.
(define kf3 (make-kegelfuss 6 6))


;; ---------------------------------------------------------------------
;; Aufgabenteil (b)
;; ---------------------------------------------------------------------

;; Signatur: volumen-fuss : regalfuss -> number
;; Zweck: Berechnet das Volumen eines Regalfusses in cm^3 ohne Nachkommastellen.
;;    Fehler, wenn kein gueltiger Regalfuss uebergeben wurde.
;; Beispiele:
;;    (volumen-fuss qf1) sollte  160 ergeben.
;;    (volumen-fuss qf2) sollte  715 ergeben.
;;    (volumen-fuss qf3) sollte  512 ergeben.
;;    (volumen-fuss zf1) sollte  754 ergeben.
;;    (volumen-fuss zf2) sollte 1244 ergeben.
;;    (volumen-fuss zf3) sollte  785 ergeben.
;;    (volumen-fuss kf1) sollte  263 ergeben.
;;    (volumen-fuss kf2) sollte  275 ergeben.
;;    (volumen-fuss kf3) sollte   56 ergeben.
;;    (volumen-fuss 'Hallo) sollte einen Fehler erzeugen.
;; Schablone:
;;   Signatur: volumen-fuss : regalfuss -> ???
;;   (define (volumen-fuss a-regalfuss)
;;        ( ... (quaderfuss a-regalfuss) ... )
;;        ( ... (zylinderfuss a-regalfuss) ... )
;;        ( ... (kegelfuss a-regalfuss) ) ... ) )
;; Definition:
(define (volumen-fuss a-regalfuss)
  (cond [(quaderfuss? a-regalfuss ) (floor(calcQuaderVolumen a-regalfuss))]
        [(zylinderfuss? a-regalfuss ) (floor(calcZylinderVolumen a-regalfuss))]
        [(kegelfuss? a-regalfuss ) (floor(calcKegelVolumen a-regalfuss))]
        [else (error 'volumen-fuss "Kein gueltiger Regalfuss uebergeben.")]))
     
;; Tests: [siehe Hinweise in Aufgabenstellung zur Verwendung von check-within]
(check-within (volumen-fuss qf1)  160 0.1)
(check-within (volumen-fuss qf2)  715 0.1)
(check-within (volumen-fuss qf3)  512 0.1)
(check-within (volumen-fuss zf1)  754 0.1)
(check-within (volumen-fuss zf2) 1244 0.1)
(check-within (volumen-fuss zf3)  785 0.1)
(check-within (volumen-fuss kf1)  263 0.1)
(check-within (volumen-fuss kf2)  275 0.1)
(check-within (volumen-fuss kf3)   56 0.1)
(check-error  (volumen-fuss 'Hallo) "volumen-fuss: Kein gueltiger Regalfuss uebergeben.")

;; ---------------------------------------------------------------------

;; [Hier Hilfsfunktionen fuer (b) definieren]

(define (calcQuaderVolumen a-quaderfuss) ;; Berechne das Volume eines gegebenen Quaders (Formel: Breite*Höhe*Tiefe)
  (* (quaderfuss-breite a-quaderfuss) (quaderfuss-hoehe a-quaderfuss) (quaderfuss-tiefe a-quaderfuss)))

(check-expect (calcQuaderVolumen qf1) 160)

;; ---------------------------------------------------------------------

(define (calcZylinderVolumen a-zylinderfuss) ;; Berechne das Volume eines gegebenen Zylinder (Formel: Grundfläche*Höhe)
  (* (calcRundeGrundfläche (zylinderfuss-durchmesser a-zylinderfuss)) (zylinderfuss-hoehe a-zylinderfuss))) 

(check-within (calcZylinderVolumen zf1) 754 0.1)

;; ---------------------------------------------------------------------

(define (calcKegelVolumen a-kegelfuss) ;; Berechne das Volume eines gegebenen Kegel (Formel: 1/3*Grundfläche*Höhe)
  (* (/ 1 3) (calcRundeGrundfläche (kegelfuss-durchmesser a-kegelfuss)) (kegelfuss-hoehe a-kegelfuss)))

 (check-within (calcKegelVolumen kf1) 264 0.2)

;; ---------------------------------------------------------------------

(define  (calcRundeGrundfläche durchmesser) ;; Berechen Fläche eines Kreises mit Durchmesser (Formel: (r^2)*pi)
  (* (/ durchmesser 2) (/ durchmesser 2) PI))

(check-within (calcRundeGrundfläche 4) 12.5 0.1)

;; ---------------------------------------------------------------------
;; Aufgabenteil (c)
;; ---------------------------------------------------------------------

;; Signatur: flaeche-fuss : regalfuss -> number
;; Zweck: Berechnet die Oberflaeche eines Regalfusses in cm^2 ohne Nachkommastellen.
;;    Fehler, wenn kein gueltiger Regalfuss uebergeben wurde.
;; Beispiele:
;;    (flaeche-fuss qf1) sollte  192 ergeben.
;;    (flaeche-fuss qf2) sollte  526 ergeben.
;;    (flaeche-fuss qf3) sollte  384 ergeben.
;;    (flaeche-fuss zf1) sollte  477 ergeben.
;;    (flaeche-fuss zf2) sollte  640 ergeben.
;;    (flaeche-fuss zf3) sollte  471 ergeben.
;;    (flaeche-fuss kf1) sollte  286 ergeben.
;;    (flaeche-fuss kf2) sollte  258 ergeben.
;;    (flaeche-fuss kf3) sollte   91 ergeben.
;;    (flaeche-fuss 'Hallo) sollte einen Fehler erzeugen.
;; Schablone:
;;   Signatur: flaeche-fuss : regalfuss -> ???
;;  (define (flaeche-fuss a-regalfuss)
;;      ( ... (quaderfuss a-regalfuss) ...)
;;      ( ... (zylinderfuss a-regalfuss) ...)
;;      ( ... (kegelfuss a-regalfuss) ...) )
;; Definition:
(define (flaeche-fuss a-regalfuss)
  (cond [(quaderfuss? a-regalfuss ) (floor(calcQuaderFläche a-regalfuss))]
        [(zylinderfuss? a-regalfuss ) (floor(calcZylinderFläche a-regalfuss))]
        [(kegelfuss? a-regalfuss ) (floor(calcKegelFläche a-regalfuss))]
        [else (error 'flaeche-fuss "Kein gueltiger Regalfuss uebergeben.")]))
     
;; Tests: [siehe Hinweise in Aufgabenstellung zur Verwendung von check-within]
(check-within (flaeche-fuss qf1) 192 0.1)
(check-within (flaeche-fuss qf2) 526 0.1)
(check-within (flaeche-fuss qf3) 384 0.1)
(check-within (flaeche-fuss zf1) 477 0.1)
(check-within (flaeche-fuss zf2) 640 0.1)
(check-within (flaeche-fuss zf3) 471 0.1)
(check-within (flaeche-fuss kf1) 286 0.1)
(check-within (flaeche-fuss kf2) 258 0.1)
(check-within (flaeche-fuss kf3)  91 0.1)
(check-error  (flaeche-fuss 'Hallo) "flaeche-fuss: Kein gueltiger Regalfuss uebergeben.")

;; ---------------------------------------------------------------------

;; [Hier Hilfsfunktionen fuer (c) definieren]


(define (calcQuaderFläche a-quaderfuss) ;; Berechnung der Fläche eines Quaders (Formel: 2h*t+2t*b+2b*h)
  (+ (* (quaderfuss-hoehe a-quaderfuss) (quaderfuss-tiefe a-quaderfuss) 2) (* (quaderfuss-tiefe a-quaderfuss) (quaderfuss-breite a-quaderfuss) 2) (* (quaderfuss-breite a-quaderfuss) (quaderfuss-hoehe a-quaderfuss) 2)))

(check-within (calcQuaderFläche qf1) 192 0.1)


(define (calcZylinderFläche a-zylinderfuss) ;; Berechnung der Fläche eines Zylinders (Formel: Grundfläche*Mantelfläche )
  (+ (* (calcRundeGrundfläche (zylinderfuss-durchmesser a-zylinderfuss)) 2) (calcZylinderMantel a-zylinderfuss (zylinderfuss-durchmesser a-zylinderfuss) (zylinderfuss-hoehe a-zylinderfuss))))

(check-within (calcZylinderFläche zf1) 477.5 0.1)


(define (calcZylinderMantel a-zylinderfuss durchmesser hoehe) ;; Berechne die Mantelfläche eines Zylinders (Formel: 2*pi*r*h)
  (* 2 PI (/ (zylinderfuss-durchmesser a-zylinderfuss) 2) hoehe))

(check-within (calcZylinderMantel zf1 3 5) 125.6 0.1)

(define (calcKegelFläche a-kegelfuss) ;; Berechnung der Fläche eines Kegels (Formel: Grundfläche+Mantelfläche )
  (+ (calcRundeGrundfläche (kegelfuss-durchmesser a-kegelfuss)) (calcKegelMantelFläche a-kegelfuss (kegelfuss-durchmesser a-kegelfuss) (kegelfuss-hoehe a-kegelfuss))))

(check-within (calcKegelFläche kf1) 287 0.1)

(define (calcKegelMantelFläche a-kegelfuss durchmesser hoehe) ;; Berechen Kegel Mantel Fläche (Formel: pi*wurzel(h^2+d^2) )
  (* PI (/ (kegelfuss-durchmesser a-kegelfuss) 2) (sqrt (+ (hochZwei (kegelfuss-hoehe a-kegelfuss)) (hochZwei (/ (kegelfuss-durchmesser a-kegelfuss) 2))))))

(check-within (calcKegelMantelFläche kf1 5 7) 173.8 0.1)

(define (hochZwei zahl) ;; Rechnet eine gegebene Zahl hoch 2
  (* zahl zahl))

(check-within (hochZwei 2) 4 0.1)

;; Schon definiert (s.o.)
;; (define  (calcRundeGrundfläche durchmesser)
;;  (* (/ durchmesser 2) (/ durchmesser 2) pi))
