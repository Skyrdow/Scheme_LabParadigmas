#lang racket

(require "otras_funciones.rkt")
(provide (all-defined-out))


;;; TDA pixel

;; selectores genericos de pixeles
; Entrada: Pixel
; Salida: String
; Descripción: retorna el tipo del pixel
(define (getPixType pix) (first pix))
; Entrada: Pixel
; Salida: Entero
; Descripción: retorna la coordenada X del pixel
(define (getPixPosX pix) (second pix))
; Entrada: Pixel
; Salida: Entero
; Descripción: retorna la coordenada Y del pixel
(define (getPixPosY pix) (third pix))
; Entrada: Pixel
; Salida: Entero | Lista de Enteros | String
; Descripción: retorna el valor de color que almacena el pixel
(define (getPixVal pix) (fourth pix))
; Entrada: Pixel
; Salida: Entero
; Descripción: retorna la profundidad del pixel
(define (getPixDepth pix)(fifth pix))

;; modificadores genericos de pixeles
; Entrada: Pixel X Valor (String)
; Salida: Pixel
; Descripción: retorna un pixel idéntico, con el valor de tipo modificado
(define (setPixType pix val) (list val (getPixPosX pix) (getPixPosY pix) (getPixVal pix) (getPixDepth pix)))
; Entrada: Pixel X Valor (Entero)
; Salida: Pixel
; Descripción: retorna un pixel idéntico, con el valor de la coordenada x modificado
(define (setPixPosX pix val) (list (getPixType pix) val (getPixPosY pix) (getPixVal pix) (getPixDepth pix)))
; Entrada: Pixel X Valor (Entero)
; Salida: Pixel
; Descripción: retorna un pixel idéntico, con el valor de la coordenada y modificado
(define (setPixPosY pix val) (list (getPixType pix) (getPixPosX pix) val (getPixVal pix) (getPixDepth pix)))
; Entrada: Pixel X Valor (Entero | Lista de 3 Enteros | String)
; Salida: Pixel
; Descripción: retorna un pixel idéntico, con el valor de color modificado
(define (setPixVal pix val) (list (getPixType pix) (getPixPosX pix) (getPixPosY pix) val (getPixDepth pix)))
; Entrada: Pixel X Valor (Entero)
; Salida: Pixel
; Descripción: retorna un pixel idéntico, con el valor de profundidad modificado
(define (setPixDepth pix val) (list (getPixType pix) (getPixPosX pix) (getPixPosY pix) (getPixVal pix) val))
; Entrada: Pixel X Valor x X Valor y
; Salida: Pixel
; Descripción: retorna un pixel idéntico, con ambas coordenadas x e y modificadas
(define (setPixXY pix x y) (list (getPixType pix) x y (getPixVal pix) (getPixDepth pix)))

;; otras funciones
; Entrada: String X Pixel
; Salida: Booleano
; Descripción: función generica que compara el tipo de un pixel
(define (compType type pix) (equal? type (getPixType pix)))
; Entrada: Pixel
; Salida: Pixel
; Descripción: intercambia coordenadas x e y de un pixel
(define (swapPixXY pix) (setPixXY  pix (getPixPosY pix) (getPixPosX pix)))
; Entrada: Pixrgb
; Salida: Pixhex
; Descripción: transforma los valores (r g b) de un pixrgb, los transforma y los retorna en un pixel hexadecimal "#RRGGBB"
(define (pixrgb->pixhex pix) (setPixType (setPixVal pix (string-upcase (string-append "#" (int->hex (getR pix)) (int->hex (getG pix)) (int->hex (getB pix))))) "pixhex-d"))

  
;; TDA pixbit:
; Entrada: x (Entero) X y (Entero) X bit (Entero, 0 o 1) X profundidad (Entero, de 0 a 255)
; Salida: Pixbit
; Descripción: constructor de pixbit
(define (pixbit-d x y bit depth) (list "pixbit-d" x y bit depth))
; Entrada: Pixbit
; Salida: Bit (Entero)
; Descripción: selector del bit de pixbit
(define (getBit pixbit) (getPixVal pixbit))
; Entrada: Pixbit | Elemento Basado en Lista
; Salida: Booleano
; Descripción: pertenencia de pixbit
(define (pixbit? pix) (compType "pixbit-d" pix))
; Entrada: Pixbit
; Salida: String
; Descripción: transforma el valor de color de un pixbit a string, si entra #F retorna "#------" (el pixel no existe)
(define (pixbit->string pix) (if (false? pix)
                                 "#------"
                                 (if (equal? 0 (getBit pix))
                                     "0"
                                     "1")))

; Entrada: Pixbit
; Salida: Pixbit
; Descripción: invierte el valor del bit
(define (invertColorBit pix) (setPixVal pix (- 1 (getPixVal pix))))


;; TDA pixrgb:
; Entrada: x (Entero) X y (Entero) X r (Entero, de 0 a 255) X g (Entero, de 0 a 255) X b (Entero, de 0 a 255) X profundidad (Entero, de 0 a 255)
; Salida: pixrgb
; Descripción: constructor de pixrgb
(define (pixrgb-d x y r g b depth) (list "pixrgb-d" x y (list r g b) depth))
; Entrada: Pixrgb
; Salida: Lista de Enteros
; Descripción: selector de la lista que almacena los valores RGB
(define (getRgb pixrgb) (getPixVal pixrgb))
; Entrada: Pixrgb
; Salida: Entero
; Descripción: retorna el valor R del pixel
(define (getR pix) (first (getRgb pix)))
; Entrada: Pixrgb
; Salida: Entero
; Descripción: retorna el valor G del pixel
(define (getG pix) (second (getRgb pix)))
; Entrada: Pixrgb
; Salida: Entero
; Descripción: retorna el valor B del pixel
(define (getB pix) (third (getRgb pix)))
; Entrada: Pixrgb X valor (Entero)
; Salida: Pixrgb
; Descripción: modificador del valor R del pixel
(define (setR pix valR) (setPixVal pix (list valR (getG pix) (getB pix))))
; Entrada: Pixrgb X valor (Entero)
; Salida: Pixrgb
; Descripción: modificador del valor G del pixel
(define (setG pix valG) (setPixVal pix (list (getR pix) valG (getB pix))))
; Entrada: Pixrgb X valor (Entero)
; Salida: Pixrgb
; Descripción: modificador del valor B del pixel
(define (setB pix valB) (setPixVal pix (list (getR pix) (getG pix) valB)))

; Entrada: Pixrgb
; Salida: Booleano
; Descripción: pertenencia de pixrgb
(define (pixrgb? pix) (compType "pixrgb-d" pix))
; Entrada: Pixrgb
; Salida: String
; Descripción: retorna el valor RGB del pixel, transformandolo a pixhex primero, si entra #F retorna "#------" (el pixel no existe)
(define (pixrgb->string pix) (if (false? pix)
                                 "#------"
                                 (pixhex->string (pixrgb->pixhex pix))))

; Entrada: Pixrgb
; Salida: Pixrgb
; Descripción: invierte el color representado por los valores RGB
(define (invertColorRGB pix) (setPixVal pix (list (- 255 (getR pix)) (- 255 (getG pix)) (- 255 (getB pix)))))

; Entrada: Función getR|getG|getB X Función setR|setG|setB X Función  X Pixrgb
; Salida: pixrgb
; Tipo de Recursión: 
; Algoritmo/Estrategia: 
; Descripción: adj channel uso: (edit  (adjustChannel getR setR incCh) img)
(define adjustChannel (lambda (f1 f2 f3) (lambda (pix) (f2 pix (f3 (f1 pix))))))
; Entrada: Entero
; Salida: Entero
; Descripción: aumenta en 1 el valor ingresado
(define (incCh val) (+ 1 val))
; Entrada: Entero
; Salida: Entero
; Descripción: disminuye en 1 el valor ingresado
(define (decCh val) (- val 1))



;; TDA pixhex:
; Entrada: x (Entero) X y (Entero) X hex (String, de formato "#RRGGBB") X profundidad (Entero, de 0 a 255)
; Salida: Pixhex
; Descripción: constructor de pixhex
(define (pixhex-d x y hex depth) (list "pixhex-d" x y hex depth))
; Entrada: Pixhex
; Salida: String
; Descripción: selector del hex de pixhex
(define (getHex pixhex) (getPixVal pixhex))
; Entrada: Pixhex
; Salida: Booleano
; Descripción: pertenencia de pixhex
(define (pixhex? pix) (compType "pixhex-d" pix))
; Entrada: Pixhex | Falso
; Salida: String
; Descripción: retorna el valor de color que almacena pixhex, por el contrario, si entra #F retorna "#------" (el pixel no existe)
(define (pixhex->string pix) (if (false? pix)
                                 "#------"
                                 (getPixVal pix)))
