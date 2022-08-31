#lang racket

;(define (pixel type x y value depth) (list type x y value depth))// descartado por redundancia

;;; pixel

;; selectores genericos de pixeles
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

;; otras funciones
; función generica que compara el tipo de un pixel
(define (compType type pix) (equal? type (getPixType pix)))
; transforma numeros en base 10 a hex dejando un 0 para valores entre 0 y 16
(define (int->hex val) (if (< 16 val) (number->string val 16) (string-append "0" (number->string val 16))))
; transforma valores de (r g b) a la string de color hexadecimal "#RRGGBB"
(define (rgb->hex pix) (setPixVal pix (string-upcase (string-append "#" (int->hex (getPixR pix)) (int->hex (getPixG pix)) (int->hex (getPixB pix))))))
; intercambia coordenadas x e y de un pixel
(define (swapPixXY pix) (setPixPosX (setPixPosY pix (getPixPosX pix)) (getPixPosY pix)))

  
;; pixbit:
; constructor
(define (pixbit-d x y bit depth) (list "pixbit-d" x y bit depth))
; selector
(define (getBit pixbit) (getPixVal pixbit))
; pertenencia
(define (pixbit? pix) (compType "pixbit-d" pix))

;; pixrgb:
; constructor
(define (pixrgb-d x y r g b depth) (list "pixrgb-d" x y (list r g b) depth))
; selector
(define (getRgb pixrgb) (getPixVal pixrgb))
(define (getPixR pixrgb) (first (getRgb pixrgb)))
(define (getPixG pixrgb) (second (getRgb pixrgb)))
(define (getPixB pixrgb) (third (getRgb pixrgb)))
;pertenencia
(define (pixrgb? pix) (compType "pixrgb-d" pix))

;; pixhex:
; constructor
(define (pixhex-d x y hex depth) (list "pixhex-d" x y hex depth))
; selector
(define (getHex pixhex) (getPixVal pixhex))
; pertenencia
(define (pixhex? pix) (compType "pixhex-d" pix))

;;; image:

;; constructor
; retorno: tipo, ancho, alto, valor de compresión, lista de pixeles
(define (image width height . pixels) (list "image" width height -1 pixels))

;; selectores
(define (getImgW img) (second img))
(define (getImgH img) (third img))
(define (getImgComp img) (fourth img))
(define (getImgPixs img) (fifth img))

;; modificadores // no se usa "image" para retornar una imagen, ya que los datos de compresión se pierden
(define (setImgW img val) (list "image" val (getImgH img) (getImgComp img) (getImgPixs img)))
(define (setImgH img val) (list "image" (getImgW img) val (getImgComp img) (getImgPixs img)))
(define (setImgComp img val) (list "image" (getImgW img) (getImgH img) val (getImgPixs img)))
(define (setImgPixs img val) (list "image" (getImgW img) (getImgH img) (getImgComp img) val))

;; pertenencia
(define (image? check) (equal? "image" (first check)))

;; otras funciones
; tipo de imagen
(define (bitmap? img) (and (image? img)(pixbit? (first (getImgPixs img)))))
(define (pixmap? img) (and (image? img)(pixrgb? (first (getImgPixs img)))))
(define (hexmap? img) (and (image? img)(pixhex? (first (getImgPixs img)))))
(define (compressed? img) (equal? -1 (getImgComp img)))

; información de imagen
; histograma
(define (histogram img) (cond
                          [(bitmap? img) (histoRec (sort (pixs->val (getImgPixs img)) <) (list))]
                          [(pixmap? img) (histoRec (sort (pixs->val (getImgPixs (imgRGB->imgHex img))) string<?) (list))]
                          [(hexmap? img) (histoRec (sort (pixs->val (getImgPixs img)) string<?) (list)) ]))
; toma la lista de pixeles para recorrer recursivamente y data (valor cantidad)
(define (histoRec vals data) (if (empty? vals)
                                 data
                                 (histoRec (remove* (list (car vals)) vals) (append data (list (car vals) (count (lambda (val) (equal? (car vals) val)) vals))))))
; toma todos los pixeles y devuelve la lista de valores
(define (pixs->val img) (map (lambda (pix) (getPixVal pix)) img))

;; transformación de imagenes
(define (flipV img) (if (image? img)
                               (setImgPixs img (flipPixV (getImgPixs img) (list) (getImgW img)))
                               (display "No es una imagen")))
; funcion recursiva de apoyo
(define (flipPixV pixs flPixs imgW) (if (empty? pixs) flPixs (flipPixV (cdr pixs) (append flPixs (setPixPosX (car pixs) (abs (+ 1 (- (getPixPosX (car pixs)) imgW))))) imgW)))

(define (flipH img) (if (image? img)
                               (setImgPixs img (flipPixH (getImgPixs img) (list) (getImgH img)))
                               (display "No es una imagen")))
; funcion recursiva de apoyo
(define (flipPixH pixs flPixs imgH) (if (empty? pixs) flPixs (flipPixH (cdr pixs) (append flPixs (setPixPosY (car pixs) (abs (+ 1 (- (getPixPosY (car pixs)) imgH))))) imgH)))

; punto 1 esquina de arriba a la izquierda, punto 2 esquina de abajo a la derecha
(define (crop img x1 y1 x2 y2) (filter (lambda (pix) (if (and (<= x1 (getPixPosX pix)) (>= x2 (getPixPosX pix)) (<= y1 (getPixPosY pix)) (>= y2(getPixPosY pix)))
                                                         #t
                                                         #f))
                                         (getImgPixs img)))
; conversión rgb a hex
(define (imgRGB->imgHex imgrgb) (setImgPixs imgrgb (map (lambda (pix) (rgb->hex pix)) (getImgPixs imgrgb))))
; rotación 90 grados a la derecha, o en sentido horario
(define (rotate90 img) (flipH (setImgPixs img (map (lambda (pix) (swapPixXY pix)) (getImgPixs img)))))




















