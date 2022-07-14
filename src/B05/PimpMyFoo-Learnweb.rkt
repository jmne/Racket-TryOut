;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname PimpMyFoo-Learnweb) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Sprachstufe "Zwischenstufe mit lambda" einstellen.

;; hier nichts verändern.
(define (pimp-my-foo f x g y)
  ((f x) (g y)))

;; hier Definition ergänzen.
(define (foo z)
  ...)

;; hier nur fehlende Parameter ergänzen.
(pimp-my-foo foo ... foo ...)

;; keine weiteren Hilfsfunktionen oder zusätzliche Aufrufe definieren.