#lang racket
(require "pixel.rkt")
(require "image.rkt")

(define img1 (image 2 3 (pixrgb-d 0 0 255 0 0 10) (pixrgb-d 0 1 0 255 0 20) (pixrgb-d 1 0 0 0 255 10) (pixrgb-d 1 1 255 255 255  1)(pixrgb-d 0 2 255 255 255  1)))
(define img2 (image 2 2 (pixbit-d 0 0 0 10) (pixbit-d 0 1 1 20) (pixbit-d 1 0 1 10) (pixbit-d 1 1 0 255)))
(define img3 (imgRGB->imgHex img1))

(define img4 (crop img1 0 0 0 0)) ; debería retornar una imágen con un pixel
(define img5 (crop img2 0 0 0 1)) ; debería retornar una imágen con dos pixeles
(define img6 (crop img1 0 1 1 1)) ; debería retornar una imágen con dos pixeles
(define img7 (crop img2 0 0 1 1)) ; debería retornar la misma imagen

(define img18 (rotate90 img1))
(define img19 (rotate90 img2))
(define img20 (rotate90 img3))
(define img21 (rotate90 img4))
(define img22 (rotate90 img5))
(define img23 (rotate90 img6))
(define img24 (rotate90 img7))

(define img8 (compress img1))
(define img9 (compress img2))
(define img10 (compress img3))
(define img11 (compress img4))
(define img12 (compress img5))
(define img13 (compress img6))
(define img14 (compress img7))

(define img15 (edit invertColorBit img2))
(define img16 (edit invertColorRGB img1))

;se asume que las funciones de ajuste de canal están implementadas. 
(define img17 (edit (adjustChannel getB setB incCh) img1))

;imágenes no comprimidas
(display (image->string img3 pixhex->string))
(display "\n")
(display (image->string (flipH img3) pixhex->string))
(display "\n")
(display (image->string (flipV img3) pixhex->string))
(display "\n")
(display (image->string (rotate90 img3) pixhex->string))
(display "\n")
(display (image->string (crop img3 0 1 1 2) pixhex->string))
(display "\n")
(display (image->string img10 pixhex->string))


