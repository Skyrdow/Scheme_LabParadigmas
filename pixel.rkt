#lang racket

(require "otras_funciones.rkt")
(provide (all-defined-out))

;;; TDA pixel

;; selectores genericos de pixeles
; Entrada:
; Salida:
; Descripción:
(define (getPixType pix) (first pix))
(define (getPixPosX pix) (second pix))
(define (getPixPosY pix) (third pix))
(define (getPixVal pix) (fourth pix))
(define (getPixDepth pix)(fifth pix))

;; modificadores genericos de pixeles // no se usa pixbit, pixrgb o pixhex
(define (setPixType pix val) (list val (getPixPosX pix) (getPixPosY pix) (getPixVal pix) (getPixDepth pix)))
(define (setPixPosX pix val) (list (getPixType pix) val (getPixPosY pix) (getPixVal pix) (getPixDepth pix)))
(define (setPixPosY pix val) (list (getPixType pix) (getPixPosX pix) val (getPixVal pix) (getPixDepth pix)))
(define (setPixVal pix val) (list (getPixType pix) (getPixPosX pix) (getPixPosY pix) val (getPixDepth pix)))
(define (setPixDepth pix val) (list (getPixType pix) (getPixPosX pix) (getPixPosY pix) (getPixVal pix) val))
(define (setPixXY pix x y) (list (getPixType pix) x y (getPixVal pix) (getPixDepth pix)))

;; otras funciones
; función generica que compara el tipo de un pixel
(define (compType type pix) (equal? type (getPixType pix)))
; intercambia coordenadas x e y de un pixel
(define (swapPixXY pix) (setPixXY  pix (getPixPosY pix) (getPixPosX pix)))
; transforma valores (r g b) de un pixrgb a pixel hexadecimal "#RRGGBB"
(define (pixrgb->pixhex pix) (setPixType (setPixVal pix (string-upcase (string-append "#" (int->hex (getR pix)) (int->hex (getG pix)) (int->hex (getB pix))))) "pixhex-d"))

  
;; pixbit:
; constructor
(define (pixbit-d x y bit depth) (list "pixbit-d" x y bit depth))
; selector
(define (getBit pixbit) (getPixVal pixbit))
; pertenencia
(define (pixbit? pix) (compType "pixbit-d" pix))
; pixbit->string
(define (pixbit->string pix) (if (false? pix)
                                 "#------"
                                 (if (equal? 0 (getBit pix))
                                     "#000000"
                                     "#FFFFFF")))


;; pixrgb:
; constructor
(define (pixrgb-d x y r g b depth) (list "pixrgb-d" x y (list r g b) depth))
; selector
(define (getRgb pixrgb) (getPixVal pixrgb))
(define (getR pix) (first (getRgb pix)))
(define (getG pix) (second (getRgb pix)))
(define (getB pix) (third (getRgb pix)))
; modificador
(define (setR pix valR) (setPixVal pix (list valR (getG pix) (getB pix))))
(define (setG pix valG) (setPixVal pix (list (getR pix) valG (getB pix))))
(define (setB pix valB) (setPixVal pix (list (getR pix) (getG pix) valB)))
;pertenencia
(define (pixrgb? pix) (compType "pixrgb-d" pix))
; pixrgb->string
(define (pixrgb->string pix) (if (false? pix)
                                 "#------"
                                 (pixhex->string (pixrgb->pixhex pix))))



;; pixhex:
; constructor
(define (pixhex-d x y hex depth) (list "pixhex-d" x y hex depth))
; selector
(define (getHex pixhex) (getPixVal pixhex))
; pertenencia
(define (pixhex? pix) (compType "pixhex-d" pix))

; pixhex->string
(define (pixhex->string pix) (if (false? pix)
                                 "#------"
                                 (getPixVal pix)))





