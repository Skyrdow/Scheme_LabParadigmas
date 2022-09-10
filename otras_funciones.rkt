#lang racket

(provide (all-defined-out))

; transforma numeros en base 10 a hex dejando un 0 para valores entre 0 y 16
(define (int->hex val) (if (< 16 val)
                           (number->string val 16)
                           (string-append "0" (number->string val 16))))
; de hex a rgb (r g b)
(define (hex->rgb hex) (list (hex->int (substring hex 1 3)) (hex->int (substring hex 3 5)) (hex->int (substring hex 5 7))))
; string hex a int
(define (hex->int str) (string->number str 16))
