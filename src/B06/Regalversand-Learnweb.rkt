;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Regalversand-Learnweb) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ---------------------------------------------------------------------
;; Konstanten
;; ---------------------------------------------------------------------
(define PI 3.142)


;; ---------------------------------------------------------------------
;; Datendefinitionen
;; ---------------------------------------------------------------------

;; Datendefinition
;; Ein frachtgut ist entweder ein regal oder ein regalfuss.

;; Datendefinition
(define-struct regal (breite hoehe tiefe dicke durchmesser))
;; Ein regal ist eine Struktur (make-regal breite hoehe tiefe dicke durchmesser)
;; zur Repraesentation eines Regals aus Aufgabe 9.
;; Hierbei sind breite, hoehe, tiefe, dicke und durchmesser (positive) Zahlenwerte in cm.

;; Datendefinition
;; Eine regalfuss ist entweder ein quaderfuss, ein zylinderfuss oder ein
;; kegelfuss.

;; Datendefinition Quaderfuss
(define-struct quaderfuss (breite hoehe tiefe))
;; Ein quaderfuss ist eine Struktur bestehend aus Breite, Höhe und Tiefe
;; Breite, Höhe und Tiefe sind hierbei jeweils eine positive Zahlenwerte in cm

;; Datendefinition Zylinderfuss
(define-struct zylinderfuss (durchmesser hoehe))
;; Ein zylinderfuss ist eine Struktur bestehend aus Durchmesser und Höhe
;; Durchmesser und Höhe sind hierbei jeweils eine positive Zahlenwerte in cm

;; Datendefinition Kegelfuss
(define-struct kegelfuss (durchmesser hoehe))
;; Ein kegelfuss ist eine Struktur bestehen aus Durchmesser und Höhe
;; Durchmesser und Höhe sind hierbei jeweils eine positive Zahlenwerte in cm



;; ---------------------------------------------------------------------
;; Beispiel-Datenbuendel fuer Tests
;; ---------------------------------------------------------------------
(define reg1 (make-regal 33.2 40 28 1   5))  ;; Volumen:  5434
(define reg2 (make-regal 40   50 30 1.5 7))  ;; Volumen: 11106
(define reg3 (make-regal 20   30 20 2   3))  ;; Volumen:  5317

(define qf1 (make-quaderfuss  4 10  4))      ;; Volumen: 160
(define qf2 (make-quaderfuss 13  5 11))      ;; Volumen: 715
(define qf3 (make-quaderfuss  8  8  8))      ;; Volumen: 512

(define zf1 (make-zylinderfuss  8 15))       ;; Volumen:  754
(define zf2 (make-zylinderfuss 12 11))       ;; Volumen: 1244
(define zf3 (make-zylinderfuss 10 10))       ;; Volumen:  785

(define kf1 (make-kegelfuss 12  7))          ;; Volumen: 263
(define kf2 (make-kegelfuss  9 13))          ;; Volumen: 275
(define kf3 (make-kegelfuss  6  6))          ;; Volumen:  56

(define order1   (list reg3 qf1 qf1 zf1 zf1))                                      ;; aufbaubar; Volumen: 7145
(define order2   (list reg1 kf1 reg2 kf1 kf1 kf1 kf2 kf2 kf2 kf2 qf3 qf3 zf3 zf3)) ;; aufbaubar; Volumen: 21286 
(define order3   (list kf1 kf1 kf2 kf2 qf3 qf3 zf3 reg1 reg2))                     ;; nicht aufbaubar; Volumen: 19425
(define no-order (list "Regal"))                                                   ;; ungueltig



;; ---------------------------------------------------------------------
;; Aufgabenteil (a)
;; ---------------------------------------------------------------------

;; Signatur: aufbaubar? : lof -> boolean
;; Zweck: Berechnet, ob eine als Liste lof uebergebene Bestellung mindestens vier
;;    Mal so viele Fuesse enthaelt wie Regale.
;; Beispiele:
;;    (aufbaubar? order1)   sollte  true ergeben.
;;    (aufbaubar? order2)   sollte  true ergeben.
;;    (aufbaubar? order3)   sollte false ergeben.
;;    (aufbaubar? empty)    sollte einen Fehler verursachen.
;;    (aufbaubar? zf1)      sollte einen Fehler verursachen.
;;    (aufbaubar? no-order) sollte einen Fehler verursachen.
;; Schablone:
;;    Signatur: process-list : a-list -> ???
;;       (define (process-list a-list)
;;           (cond
;;                [(not(list? a-list)) ... ]
;;                [(empty? a-list) ... ]
;;                [( ... ( ... a-list) ( ... a-list)) true]
;;                [else false]
;;       )
;;
;; Definition:

(define (aufbaubar? lof)
           (cond
                [(not(list? lof)) (error 'aufbaubar? "Ungueltige Bestellung.") ] ;; Prüfe ob Eingabe valid ist ff.
                [(empty? lof) (error 'aufbaubar? "Ungueltige Bestellung.") ]
                [(not (validOrder? lof)) (error 'aufbaubar? "Ungueltige Bestellung.") ]
                [(<= ( * 4 (berechneRegale lof 0)) (berechneFuesse lof 0)) true] ;; Wenn gezählte Regale * 4 = gezählte Füße ist, ist das Regal aufbaubar => true
                [else false] ;; Ansonste gib false aus
       )
)

;; Tests:
(check-expect (aufbaubar? order1)   true)
(check-expect (aufbaubar? order2)   true)
(check-expect (aufbaubar? order3)   false)
(check-error  (aufbaubar? empty)    "aufbaubar?: Ungueltige Bestellung.")
(check-error  (aufbaubar? zf1)      "aufbaubar?: Ungueltige Bestellung.")
(check-error  (aufbaubar? no-order) "aufbaubar?: Ungueltige Bestellung.")


  
;; Nebenfunktionen (a)
;; ---------------------------------------------------------------------
;;
;;
;; Signatur: berechneRegale : list number -> number
;;
;; Zweck: Löse rekursiv Liste auf und bestimme wie viele Elemente des Typ "regal" enthalten sind
;;
;; Beispiele: (berechneRegale order1 0) sollte 1 ergeben.
;;            (berechneRegale order3 0) sollte 2 ergeben.
;;
;; Schablone:
;;    Signatur: process-list : a-list a-number -> ???
;;    (define (process-list a-list a-number)
;;       (cond
;;             [ (empty? a-list) a-number ]
;;             [ ( ... (first a-list)) ( ... (rest a-list) ( ... a-number ... ))]
;;             [ (else ( ... (rest a-list) a-number )) ]
;;       )
;;    )
;;
;; Definition der nicht-abgesicherten internen Hilfsfunktion
  (define (berechneRegale lof anzahl)
    (cond
         [(empty? lof) anzahl] ;; Wenn Liste leer gib anzahl zurück
         [(regal? (first lof)) (berechneRegale (rest lof) (+ anzahl 1))] ;; Wenn erstes Listenelement vom Typ regal,
                                                                         ;;rufe funktion rekursiv auf und addiere 1 zu anzahl
         [else (berechneRegale (rest lof) anzahl)] ;; Rekursion
    )
  )

; Tests:

(check-expect (berechneRegale order1 0) 1)
(check-expect (berechneRegale order3 0) 2)

;; ---------------------------------------------------------------------
;;
;;
;; Signatur: berechneFuesse : list number -> number
;;
;; Zweck: Löse rekursiv Liste auf und bestimme wie viele Elemente des Typ "quaderfuss", "zylinderfuss" oder "kegelfuss" enthalten sind
;;
;; Beispiele: (berechneFuesse order1 0) sollte 4 ergeben.
;;            (berechneFuesse order3 0) sollte 7 ergeben.
;;
;; Schablone:
;;    Signatur: process-list : a-list a-number -> ???
;;    (define (process-list a-list a-number)
;;       (cond
;;             [ (empty? a-list) a-number ]
;;             [ ( or 
;;                     (... (first a-list))
;;                     (... (first a-list))
;;                     (... (first a-list)) )
;;               ( ... (rest a-list) ( ... a-number ... )) ]
;;             [ (else ( ... (rest a-list) a-number )) ]
;;       )
;;    )
;;
;; Definition der nicht-abgesicherten internen Hilfsfunktion
  (define (berechneFuesse lof anzahl)
    (cond
         [(empty? lof) anzahl] ;; Wenn Liste leer gib anzahl zurück
         [(or
              (quaderfuss? (first lof))
              (zylinderfuss? (first lof))
              (kegelfuss? (first lof)) )
              (berechneFuesse (rest lof) (+ anzahl 1))] ;; Wenn Element vom Typ "Fuss", addiere 1 zu Anzahl und rufe die Funktion rekursiv auf
         [else (berechneFuesse (rest lof) anzahl)]
    )
  )

; Tests:

(check-expect (berechneFuesse order1 0) 4)
(check-expect (berechneFuesse order3 0) 7)


;; ---------------------------------------------------------------------
;;
;;
;; Signatur: validOrder? : list -> boolean
;;
;; Zweck: Löse rekursiv Liste auf und bestimme ob es ein Element des Typ regal, zylindefuss, quaderfuss oder kegelfuss gibt
;;
;; Beispiele: (validOrder? order1) sollte richtig ergeben.
;;            (validOrder? no-order) sollte falsch ergeben.
;;
;; Schablone:
;;    Signatur: process-list : a-list -> ???
;;    (define (process-list a-list)
;;       (cond
;;             [ (empty? a-list) ... ]
;;             [ ( or 
;;                     (... (first a-list))
;;                     (... (first a-list))
;;                     (... (first a-list))
;;                     (... (first a-list)) )
;;               ( ... (rest a-list) ( ... a-number ... )) ]
;;             [ (else ( ... (rest a-list))) ]
;;       )
;;    )
;;
;; Definition der nicht-abgesicherten internen Hilfsfunktion
  (define (validOrder? lof)
    (cond
         [(empty? lof) false] ;; Wenn Liste leer gib false zurück
         [(or (quaderfuss? (first lof)) 
              (zylinderfuss? (first lof))
              (kegelfuss? (first lof))
              (regal? (first lof)) )
          true] ;; Wenn Element vom Typ Fuss oder Regal gib true zurück
         [else (validOrder? (rest lof))] ;; Rekursion
    )
  )

; Tests:

(check-expect (validOrder? order1) true)
(check-expect (validOrder? no-order) false)


;; ---------------------------------------------------------------------
;; Aufgabenteil (b)
;; ---------------------------------------------------------------------

;; Signatur: gesamtvolumen : list -> number
;; Zweck: Berechnet das gesamte Nettovolumen einer als Liste lof uebergebene Bestellung.
;; Beispiele:
;;    (gesamtvolumen order1)   sollte  7145 ergeben.
;;    (gesamtvolumen order2)   sollte 21286 ergeben.
;;    (gesamtvolumen order3)   sollte 19425 ergeben.
;;    (gesamtvolumen empty)    sollte einen Fehler verursachen.
;;    (gesamtvolumen zf1)      sollte einen Fehler verursachen.
;;    (gesamtvolumen no-order) sollte einen Fehler verursachen.
;;
;; Schablone:
;;    Signatur: process-list : a-list -> ???
;;       (define (process-list a-list)
;;           (cond
;;                [(not(list? a-list)) ... ]
;;                [(empty? a-list) ... ]
;;                [( ... ( ... a-list)) ... ]
;;                [else (... a-list ...)]
;;       )
;;
;; Definition:
(define (gesamtvolumen lof)
  (cond
                [(not(list? lof)) (error 'gesamtvolumen "Ungueltige Bestellung.") ] ;; Prüfe ob Eingabe valid ist ff.
                [(empty? lof) (error 'gesamtvolumen "Ungueltige Bestellung.") ]
                [(not (validOrder? lof)) (error 'gesamtvolumen "Ungueltige Bestellung.") ]
                [else (berechneVolumen lof 0)] ;; Rufe Funktion zum berechnen des Volumens auf
  )
)

;; Tests:
(check-expect (gesamtvolumen order1)  7145)
(check-expect (gesamtvolumen order2) 21286)
(check-expect (gesamtvolumen order3) 19425)
(check-error  (gesamtvolumen empty)    "gesamtvolumen: Ungueltige Bestellung.")
(check-error  (gesamtvolumen zf1)      "gesamtvolumen: Ungueltige Bestellung.")
(check-error  (gesamtvolumen no-order) "gesamtvolumen: Ungueltige Bestellung.")


;; ---------------------------------------------------------------------

;; Wrapper-Funktion fuer volumen-regal. In Teil (b) aufrufen.

;; Signatur: regalvolumen : regal -> number
;; Zweck: Uebergibt die Abmessungen des uebergebenen Regals an die alte
;;    Loesung von Aufgabe 9.
;; Beispiele:
;;   (regalvolumen reg1)   sollte  5434 ergeben.
;;   (regalvolumen reg2)   sollte 11106 ergeben.
;;   (regalvolumen reg3)   sollte  5317 ergeben.
;; Schablone:
;;   Signatur: process-regal : regal -> ???
;;     (define (process-regal a-regal)
;;       (... (regal-breite       a-regal) ... 
;;        ... (regal-hoehe        a-regal) ...
;;        ... (regal-tiefe        a-regal) ...))
;;        ... (regal-dicke        a-regal) ...))
;;        ... (regal-durchmesser  a-regal) ...))
;; Definition: [interne, nicht abgesicherte Funktion]
(define (regalvolumen r)
  (floor (volumen-regal (regal-breite r) (regal-hoehe r) (regal-tiefe r) (regal-dicke r) (regal-durchmesser r))))
;; Tests:
(check-expect (regalvolumen reg1)  5434)
(check-expect (regalvolumen reg2) 11106)
(check-expect (regalvolumen reg3)  5317)


;; ---------------------------------------------------------------------

;; [Platz fuer weitere Hilfsfunktionen fuer (b)]

;;
;;
;; Signatur: berechneVolumen : list number -> number
;;
;; Zweck: Löse rekursiv Liste auf und bestimme von jedem Element des Typ Fuss oder Regal das Volumen und summiere
;;
;; Beispiele: (berechneVolumen order1 0) sollte 7145 ergeben.
;;            (berechneVolumen order3 0) sollte 19425 ergeben.
;;
;; Schablone:
;;    Signatur: process-list : a-list a-number -> ???
;;    (define (process-list a-list a-number)
;;       (cond
;;             [ (empty? a-list) a-number ]
;;             [ ( or 
;;                     (... (first a-list))
;;                     (... (first a-list))
;;                     (... (first a-list)) )
;;               ( ... (rest a-list) ( ... a-number ( ... (first a-list)))) ]
;;             [ ( ... (first a-list)) ( ... (rest a-list) ( ... (... (first a-list)))) ]
;;             [ (else ( ... (rest a-list) a-number )) ]
;;       )
;;    )
;;
;; Definition der nicht-abgesicherten internen Hilfsfunktion
  (define (berechneVolumen lof anzahl)
    (cond
         [(empty? lof) anzahl] ;; Wenn Liste leer gebe anzahl zurück
         [(or
              (quaderfuss? (first lof))
              (zylinderfuss? (first lof))
              (kegelfuss? (first lof)) )
              (berechneVolumen (rest lof)
                               (+ anzahl (volumen-fuss (first lof))))] ;; Wenn Element vom Typ "Fuss", addiere Volumen
                                                                                  ;;des Fuss zu Anzahl und rufe die Funktion rekursiv auf
         [(regal? (first lof)) (berechneVolumen (rest lof)
                                                (+ anzahl (regalvolumen (first lof))))] ;; Wenn Element vom Typ "Regal", addiere Volumen
                                                                                        ;; des Regal zu Anzahl und rufe die Funktion rekursiv auf
         [else (berechneVolumen (rest lof) anzahl)] ;; Rekursion
    )
  )

; Tests:

(check-expect (berechneVolumen order1 0) 7145)
(check-expect (berechneVolumen order3 0) 19425)


;; ---------------------------------------------------------------------
;; Platz fuer wiederverwertete Aufgabe 9 und 14
;; Ab hier wird nichts korrigiert und bewertet.
;; ---------------------------------------------------------------------

;; Signatur: volumen-regal : number number number number number -> number
;; Zweck: Berechnet das Volumen des Regals, gerundet auf drei Nachkommastellen
;;   in cm^3, in Abhängigkeit von Breite b, Höhe h, Tiefe t, Dicke d und
;;   Durchmesser c (jeweils in cm).

;; Beispiele:
;;   (volumen-regal 33.2 40 28 1   5) sollte etwa  5434.6875  ergeben.
;;   (volumen-regal 40   50 30 1.5 7) sollte etwa 11106.79725 ergeben.
;;   (volumen-regal 20   30 20 2   3) sollte etwa  5317.583   ergeben.

;; Definition:
(define (volumen-regal b h t d c)
  (round-n
   (- (+ (* 2 (volume-rect t b d)) (* 2 (volume-rect t h d)) (volume-rect (+ h (* 2 d)) b d)) ;; Hier werden alle einzelnen Volumina addiert
     (* 3 (volume-zyl c d))) ;; Hier werden die drei ausgeschnittenen Zylinder von der Summe aller anderen Platten subtrahiert
   3)) 

;; Tests:
(check-within (volumen-regal 33.2 40 28 1   5)  5434.6875  0.001)
(check-within (volumen-regal 40   50 30 1.5 7) 11106.79725 0.001)
(check-within (volumen-regal 20   30 20 2   3)  5317.583   0.001)

;; ---------------------------------------------------------------------

;; Definition:
(define (volume-rect a b c) 
  (* a b c)) 

;; Definition:
(define (round-n x n)
  (/ (floor (* (expt 10 n) x)) (expt 10 n)))  

;; Definition:
(define (volume-zyl r h)
  (* PI (expt (/ r 2) 2) h)) ;; PI * (d/2)^2 * h ; d = 2r

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
;;   Signatur: process-regalfuss: regalfuss -> ???
;; (define (process-regalfuss a-regalfuss)
;;   (cond
;;     [ (quaderfuss? a-regalfuss)
;;       ... (quaderfuss-breite a-regalfuss) ... (quaderfuss-hoehe a-regalfuss) ... (quaderfuss-tiefe a-regalfuss) ... ]
;;     [ (zylinderfuss? a-regalfuss)
;;       ... (zylinderfuss-durchmesser a-regalfuss) ... (zylinderfuss-hoehe a-regalfuss) ... ]
;;     [ (kegelfuss? a-regalfuss)
;;       ... (kegelfuss-durchmesser a-regalfuss) ... (kegelfuss-hoehe a-regalfuss) ... ]))
     

;; Definition:
(define (volumen-fuss a-regalfuss)
  (cond
    [ (quaderfuss? a-regalfuss) (floor (volumen-quaderfuss (quaderfuss-breite a-regalfuss) (quaderfuss-hoehe a-regalfuss)
                                (quaderfuss-tiefe a-regalfuss)))]
    [ (zylinderfuss? a-regalfuss) (floor (volumen-zylinderfuss (zylinderfuss-durchmesser a-regalfuss) (zylinderfuss-hoehe a-regalfuss)))]
    [ (kegelfuss? a-regalfuss) (floor (volumen-kegelfuss (kegelfuss-durchmesser a-regalfuss)  (kegelfuss-hoehe a-regalfuss)))]
    [ else (error 'volumen-fuss "Kein gueltiger Regalfuss uebergeben.")]))
    
     
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
;; Signatur: radius: number -> number
;; Zweck: Berechnung vom Radius (in cm), also Durchmesser durch 2
;; Beispiele:
;; (radius 2) sollte 1 ergeben.
;; (radius 4) sollte 2 ergeben.
;; Definition:
(define (radius durchmesser)
  (/ durchmesser 2))

;; Tests:
(check-expect (radius 2) 1)
(check-expect (radius 4) 2)

;; Signatur: volumen-quaderfuss: number number number -> number
;; Zweck: Berechnung des Volumens (in cm^3) eines Regalfußes in Quaderform
;; mithilfe der Breite, Höhe und Tiefe
;; Beispiele:
;; (volumen-quaderfuss 1 2 3) sollte 6 ergeben.
;; (volumen-quaderfuss 5 10 15) sollte 750 ergeben.
;; Definition:
(define (volumen-quaderfuss breite hoehe tiefe)
  (* breite hoehe tiefe))

;; Tests:
(check-expect (volumen-quaderfuss 1 2 3) 6)
(check-expect (volumen-quaderfuss 5 10 15) 750)

;; Signatur: volumen-zylinderfuss: number number -> number
;; Zweck: Berechnung des Volumens (in cm^3) eines Regalfußes in Zylinderform
;; mithilfe des Durchmessers und der Höhe
;; Beispiele:
;; (volumen-zylinderfuss 10 20) sollte etwa 1571 ergeben.
;; (volumen-zylinderfuss 25 100) sollte etwa 49093.8 ergeben.
;; Definition:
(define (volumen-zylinderfuss durchmesser hoehe)
  (* PI (sqr (radius durchmesser)) hoehe))

;; Tests:
(check-expect (volumen-zylinderfuss 10 20) 1571)
(check-within (volumen-zylinderfuss 25 100) 49093.8 0.1)

;; Signatur: volumen-kegelfuss: number number -> number
;; Zweck: Berechnung des Volumens (in cm^3) eines Regalfußes in Kegelform
;; mithilfe des Durchmessers und der Höhe
;; Beispiele:
;; (volumen-kegelfuss 10 20) sollte etwa 523.6 ergeben.
;; (volumen-kegelfuss 25 100) sollte etwa 16364.6 ergeben.
;; Definition:
(define (volumen-kegelfuss durchmesser hoehe)
  (* PI (/ 1 3) (sqr (radius durchmesser)) hoehe))

;; Tests:
(check-within (volumen-kegelfuss 10 20) 523.6 0.1)
(check-within (volumen-kegelfuss 25 100) 16364.6 0.1)
