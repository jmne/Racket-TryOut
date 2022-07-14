;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Sets-Learnweb) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ---------------------------------------------------------------------

;; Datenanalyse und -definition:
;; Ein set ist eine Struktur (make-set elements). Hierbei
;; ist elements eine Liste von Elementen.

(define-struct set (elements))

;; Beispiele:
(define set0 (make-set empty))
(define set1 (make-set (list 1)))
(define set2 (make-set (list 1 4 6 2235 231)))
(define set3 (make-set (list 5 5 1 315 2)))


;;=======================================================================================

;; Aufgabe (a)

;; Hauptfunktion
;;
;; Signatur: valid-set? : set -> boolean
;;
;; Zweck: Überprüfe ob gegebener Parameter ein Set ist der eine Liste enthält, sowie kein Element der Liste doppelt ist
;;
;; Beispiele: (valid-set? set1) sollte richtig ergeben.
;;            (valid-set? set3) sollte falsch ergeben.
;;
;; Schablone:
;;   Signatur: process-set : a-set -> ???
;;   (define (process-set a-set)
;;       (cond
;;             [(not (set? a-set) ... ]
;;             [(not (list? (set-elements a-set)) ... ]
;;             [else ( ... (set-elements a-set)) ...]
;;   )   )


;; Definition der abgesicherten Schnittstellen-Funktion
(define (valid-set? set)
  (cond [(not (set? set)) false ] ;; Ist vom Typ Set?
        [(not (list? (set-elements set))) false ] ;; Set behinhaltet List?
        [else (resolveList (set-elements set))])) ;; Führe Funktion aus und übergebe Liste aus Set

;; Tests

(check-expect (valid-set? set1) true)
(check-expect (valid-set? set3) false)

;;=======================================================================================

;; Aufgabe (b)

;; Hauptfunktion
;;
;; Signatur: contains? : list item -> boolean
;;
;; Zweck: Überprüfe rekursiv ob gegebenes Element (item) in gegebener Liste (list) vorhanden ist
;;
;; Beispiele: (contains? (list 1 2 3 4 5) 5) sollte richtig ergeben.
;;            (contains? (list 1 2 3 4 5) 6) sollte falsch ergeben.
;;            (contains? "hallo" 2) sollte "contains?: Keine Liste übergeben." ergeben.
;;
;; Schablone:
;;    Signatur: process-list : a-list a-item -> ???
;;    (define (process-list a-list a-item)
;;       (cond
;;             [ (not (list? a-list) ... ]
;;             [ (empty? list) ... ]
;;             [ (else ( ... a-list a-item)) ]
;;       )
;;    )


;; Definition der abgesicherten Schnittstellen-Funktion
(define (contains? list item)
  (cond
        [(not (list? list)) (error 'contains? "Keine Liste übergeben.") ] ;; Prüfe ob Parameter vom Typ Liste
        [(empty? list) false ] ;; Prüfe ob leer
        [else (containsElement? list item)] ;; Übergebe in Funktion Element "item" und list
  )
)

;; Tests

(check-expect (contains? (list 1 2 3 4 5) 5) true)
(check-expect (contains? (list 1 2 3 4 5) 6) false)
(check-error (contains? "hallo" 2) "contains?: Keine Liste übergeben.")


;;=========================================================================================

;; Aufgabe (c)
 
;; Hauptfunktion
;;
;; Signatur: subset? : list list -> boolean
;;
;; Zweck: Überprüfe rekursiv ob für zwei gegebene Mengen M1 und M2, M1 ⊆ M2 gilt.
;;
;; Beispiele: (subset? (list 1 2 3 4 5) (list 1 2 3 4 5 6 7)) sollte richtig ergeben.
;;            (subset? (list 1 2 3 4 5) (list 2 3 4 5 6 7 8)) sollte falsch ergeben.
;;            (subset? "hallo" 2) sollte "subset?: Keine Liste übergeben." ergeben.
;;
;; Schablone:
;;    Signatur: process-list : a-list a-list -> ???
;;    (define (process-list a-list a-list)
;;       (cond
;;             [ (not (and (list? a-list) (list? a-list)) ... ]
;;             [ (empty? a-list) ... ]
;;             [ ( ... a-list (first a-list)) ( ... (rest a-list) a-list)]
;;             [ else ... ]
;;       )
;;    )
;;
;; Definition der abgesicherten Schnittstellen-Funktion
(define (subset? menge1 menge2)
  (cond  [(not (and (list? menge1) (list? menge2))) (error 'subset? "Keine Liste übergeben.") ] ;; Prüfe ob Parameter vom Typ "list"
         [(empty? menge1) true] ;; Prüfe ob leer
         [(containsElement? menge2 (first menge1)) (subset? (rest menge1) menge2)] ;; Übergebe menge1 und menge2 in Funktion zur Überprüfung
         [else false]))

;; Tests
 
(check-expect (subset? (list 1 2 3 4 5) (list 1 2 3 4 5 6 7)) true)
(check-expect (subset? (list 1 2 3 4 5) (list 2 3 4 5 6 7 8)) false)
(check-error (subset? "hallo" 2) "subset?: Keine Liste übergeben.")

;;=========================================================================================

;; Aufgabe (d)
 
;; Hauptfunktion
;;
;; Signatur: equal-set? : list list -> boolean
;;
;; Zweck: Überprüfe rekursiv ob für zwei gegebene Mengen M1 und M2, M1 = M2 gilt.
;;
;; Beispiele: (equal-set? (list 1 2 3 4 5) (list 1 2 3 4 5)) sollte richtig ergeben.
;;            (equal-set? (list 1 2 3 4 5) (list 1 2 3 4 5 6)) sollte falsch ergeben.
;;            (equal-set? "hallo" 2) sollte "equal-set?: Keine Liste übergeben." ergeben.
;;
;; Schablone:
;;    Signatur: process-list : a-list a-list -> ???
;;    (define (process-list a-list a-list)
;;       (cond
;;             [ (not (and (list? a-list) (list? a-list)) ... ]
;;             [ (not ( ... a-list a-list)) ... ]
;;             [ (= (length a-list) (length a-list)) ...]
;;             [ else ... ]
;;       )
;;    )
;;
;; Definition der abgesicherten Schnittstellen-Funktion
(define (equal-set? menge1 menge2)
  (cond [(not (and (list? menge1) (list? menge2))) (error 'equal-set? "Keine Liste übergeben.") ] ;; Prüfe ob Parameter vom Typ "list"
        [(not (subset? menge1 menge2)) false ] ;; Prüfe ob M1 ⊆ M2
        [(= (length menge1) (length menge2)) true] ;; Wenn Teilmenge, vergleiche Länge der Mengen (wenn gleich ist M1 = M2)
        [else false])) ;; sonst falsch

;; Tests
 
(check-expect (equal-set? (list 1 2 3 4 5) (list 1 2 3 4 5)) true)
(check-expect (equal-set? (list 1 2 3 4 5) (list 1 2 3 4 5 6)) false)
(check-error (equal-set? "hallo" 2) "equal-set?: Keine Liste übergeben.")



;; Nebenfunktionen (a+b+c+d):
;;=======================================================================================

;;
;; Signatur: resolveList : list -> boolean
;;
;; Zweck: Löse rekursiv Liste auf und bestimme ob es ein doppeltes Element gibt
;;
;; Beispiele: (resolveList (list 1 2 3 4 2)) sollte falsch ergeben.
;;            (resolveList (list 1 2 3 4 5)) sollte richtig ergeben.
;;
;; Schablone:
;;    Signatur: process-list : a-list -> ???
;;    (define (process-list a-list)
;;       (cond
;;             [ (empty? a-list) ... ]
;;             [ ( ... (rest a-list) (first a-list)) ... ]
;;             [ (else ( ... (rest a-list))) ]
;;       )
;;    )
;;
;; Definition der nicht-abgesicherten internen Hilfsfunktion
(define (resolveList list)
  (cond
        [(empty? list) true] ;; Prüfe ob Liste leer
        [(containsElement? (rest list) (first list)) false] ;; Prüfe ob erstes Listenelement in rest der Liste vorhanden ist
        [else (resolveList (rest list))] ;; Rekursion
  )
)

; Tests:

(check-expect (resolveList (list 1 2 3 4 2)) false)
(check-expect (resolveList (list 1 2 3 4 5)) true)

;;=======================================================================================

;; Signatur: resolveList : list -> boolean
;;
;; Zweck: Überprüfe rekursiv ob gegebenes Element (item) in gegebener Liste (list) vorhanden ist
;;
;; Beispiele: (containsElement? (list 1 2 3 4 5) 4) sollte falsch ergeben.
;;            (containsElement? (list 1 2 3 4 5) 9) sollte richtig ergeben.
;;
;; Schablone:
;;    Signatur: process-list : a-list a-item -> ???
;;    (define (process-list a-list a-item)
;;       (cond
;;             [ (empty? a-list) ... ]
;;             [ (equal (first a-list) a-item) ... ]
;;             [ (else ( ... (rest a-list) a-item)) ]
;;       )
;;    )
;;
;; Definition der nicht-abgesicherten internen Hilfsfunktion
(define (containsElement? list item)
  (cond
        [(empty? list) false] ;; Prüfe ob Liste leer ist
        [(equal? (first list) item) true] ;; Prüfe ob Element (item) = erstes Element der Liste
        [else (containsElement? (rest list) item)] ;; Rekursion
  )
)

;; Tests:

(check-expect (containsElement? (list 1 2 3 4 5) 4) true)
(check-expect (containsElement? (list 1 2 3 4 5) 9) false)