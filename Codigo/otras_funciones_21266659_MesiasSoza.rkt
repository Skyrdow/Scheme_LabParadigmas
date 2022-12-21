#lang racket

(provide (all-defined-out))

; Entrada: Entero
; Salida: String
; Descripción: transforma numeros en base 10 a hex dejando un 0 para valores entre 0 y 16
(define (int->hex val) (if (< 16 val)
                           (number->string val 16)
                           (string-append "0" (number->string val 16))))
; Entrada: String
; Salida: Lista de 3 enteros
; Descripción: transforma de hex (#RRGGBB) a (r g b)
(define (hex->rgb hex) (list (hex->int (substring hex 1 3)) (hex->int (substring hex 3 5)) (hex->int (substring hex 5 7))))
; Entrada: String
; Salida: Entero
; Descripción: transforma de string hex a int
(define (hex->int str) (string->number str 16))
