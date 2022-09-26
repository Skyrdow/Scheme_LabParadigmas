#lang racket
(require "pixel.rkt")
(require "image.rkt")

(define imgextra1 (image 2 2
                    (pixrgb-d 0 0 60 1 1 10)
                    (pixrgb-d 0 1 2 80 2 20)
                    (pixrgb-d 1 0 5 3 60 10)
                    (pixrgb-d 1 1 145 90 123  1)))

(define imgextra2 (image 2 2
                  (pixbit-d 0 0 0 10)
                  (pixbit-d 0 1 1 20)
                  (pixbit-d 1 0 1 10)
                  (pixbit-d 1 1 0 255)))

(define imgextra3 (imgRGB->imgHex imgextra1))

(define imgextra4 (image 2 3
                  (pixbit-d 0 0 0 10)
                  (pixbit-d 0 1 1 20)
                  (pixbit-d 1 0 1 10)
                  (pixbit-d 0 2 0 10)
                  (pixbit-d 1 2 1 10)
                  (pixbit-d 1 1 0 255)))

(define imgextra5 (image 3 1
                  (pixrgb-d 0 0 10 20 30 10)
                  (pixrgb-d 1 0 40 50 60 20)
                  (pixrgb-d 2 0 80 90 100 10)))

;imprimir una representación string de las imagenes
(display "Funciones de prueba con los 3 ejemplos extra requeridos\nImg1 (RGB): \n")
(display (image->string imgextra1 pixrgb->string))

(display "\nimgextra2 (BIT): \n")
(display (image->string imgextra2 pixbit->string))

(display "\nimgextra3 (HEX): \n")
(display (image->string imgextra3 pixhex->string))
(display "\nimgextra4 (BIT): \n")
(display (image->string imgextra4 pixbit->string))
(display "\nimgextra5 (RGB): \n")
(display (image->string imgextra5 pixrgb->string))


(display "\n## Pertenencias:\n\n# Bitmap: \n")
(display "\nimgextra1: \n")
(bitmap? imgextra1)
(display "\nimgextra2: \n")
(bitmap? imgextra2)
(display "\nimgextra3: \n")
(bitmap? imgextra3)

(display "\n# Pixmap: \n")
(display "\nimgextra1: \n")
(pixmap? imgextra1)
(display "\nimgextra2: \n")
(pixmap? imgextra2)
(display "\nimgextra3: \n")
(pixmap? imgextra3)

(display "\n# Hexmap: \n")
(display "\nimgextra1: \n")
(hexmap? imgextra1)
(display "\nimgextra2: \n")
(hexmap? imgextra2)
(display "\nimgextra3: \n")
(hexmap? imgextra3)

(display "\n# Compressed?: \n")
(display "\nimgextra1: \n")
(compressed? imgextra1)
(display "\nimgextra2: \n")
(compressed? imgextra2)
(display "\nimgextra3: \n")
(compressed? imgextra3)

(display "\n## Flips: \n")
(display "\n# FlipH: \n")
(display "\nimgextra1: \n")
(display (image->string (flipH imgextra1) pixrgb->string))
(display "\nimgextra2: \n")
(display (image->string (flipH imgextra2) pixbit->string))
(display "\nimgextra3: \n")
(display (image->string (flipH imgextra3) pixhex->string))

(display "\n# FlipV: \n")
(display "\nimgextra1: \n")
(display (image->string (flipV imgextra1) pixrgb->string))
(display "\nimgextra2: \n")
(display (image->string (flipV imgextra2) pixbit->string))
(display "\nimgextra3: \n")
(display (image->string (flipV imgextra3) pixhex->string))

(display "\n## Crop: \n")
(display "\n# imgextra1, p1=0,0 p2=0,0 \n")
(display (image->string (crop imgextra1 0 0 0 0) pixrgb->string))
(display "\n# imgextra2, p1=0,0 p2=0,1 \n")
(display (image->string (crop imgextra2 0 0 0 1) pixbit->string))
(display "\n# imgextra3, p1=0,1 p2=1,1 \n")
(display (image->string (crop imgextra3 0 1 1 1) pixhex->string))

(display "\n## imgRGB->imgHex:\n")
(display "\n# imgextra1 -> hex \n")
(display (image->string (imgRGB->imgHex imgextra1) pixhex->string))
(display "\n# Crop imgextra1, p1=0,0 p2=1,0 -> hex \n")
(display (image->string (imgRGB->imgHex (crop imgextra1 0 0 1 0)) pixhex->string))
(display "\n# imgextra5 -> hex \n")
(display (image->string (imgRGB->imgHex imgextra5) pixhex->string))

(display "\n\n## histogram: \n")
(display "\n# imgextra1: \n")
(histogram imgextra1)
(display "\n# imgextra2: \n")
(histogram imgextra2)
(display "\n# imgextra3: \n")
(histogram imgextra3)

(display "\n\n## rotate90: \n")
(display "\n# imgextra1: \n")
(display (image->string (rotate90 imgextra1) pixrgb->string))
(display "\n# imgextra2: \n")
(display (image->string (rotate90 imgextra2) pixbit->string))
(display "\n# imgextra3: \n")
(display (image->string (rotate90 imgextra3) pixhex->string))

(display "\n\n## compress: \n")
(display "\n# imgextra1: \n")
(compress imgextra1)
(display "\n# imgextra2: \n")
(compress imgextra2)
(display "\n# imgextra3: \n")
(compress imgextra3)
(display "\n# Compressed?: \n")
(display "\n# imgextra1: \n")
(compressed? (compress imgextra1))
(display "\n# imgextra2: \n")
(compressed? (compress imgextra2))
(display "\n# imgextra3: \n")
(compressed? (compress imgextra3))

(define imgextra8 (compress imgextra1))
(define imgextra9 (compress imgextra2))
(define imgextra10 (compress imgextra3))

(display "\n\n## edit: \n")
(display "\n\n# invertColorBit: \n")
(display "\n# imgextra2: \n")
(display (image->string (edit invertColorBit imgextra2) pixbit->string))
(display "\n# crop imgextra2 p1=0,0 p2=0,1: \n")
(display (image->string (edit invertColorBit (crop imgextra2 0 0 0 1)) pixbit->string))
(display "\n# imgextra4: \n")
(display (image->string (edit invertColorBit imgextra4) pixbit->string))

(display "\n\n# invertColorRgb: \n")
(display "\n# imgextra1: \n")
(display (image->string (edit invertColorRGB imgextra1) pixrgb->string))
(display "\n# crop imgextra1 p1=0,0 p2=0,1: \n")
(display (image->string (edit invertColorRGB (crop imgextra1 0 0 0 1)) pixrgb->string))
(display "\n# imgextra5: \n")
(display (image->string (edit invertColorRGB imgextra5) pixrgb->string))

(display "\n\n# adjustChannel: \n")
(display "\n# imgextra1, getR setR decCh: \n")
(display (image->string (edit (adjustChannel getR setR decCh) imgextra1) pixrgb->string))
(display "\n# imgextra1, getG setG decCh: \n")
(display (image->string (edit (adjustChannel getG setG decCh) imgextra1) pixrgb->string))
(display "\n# imgextra1, getB setB decCh: \n")
(display (image->string (edit (adjustChannel getB setB decCh) imgextra1) pixrgb->string))

(display "\n\n## image->string: \n")
(display "\nimgextra1 (BIT): \n")
(display (image->string imgextra1 pixrgb->string))
(display "\nimgextra2 (BIT): \n")
(display (image->string imgextra2 pixbit->string))
(display "\nimgextra3 (HEX): \n")
(display (image->string imgextra3 pixhex->string))

(display "\n\n## depthLayers: \n")
(display "\nimgextra1: \n")
(depthLayers imgextra1)
(display "\nimgextra2: \n")
(depthLayers imgextra2)
(display "\nimgextra3: \n")
(depthLayers imgextra3)


(display "\n\n## decompress: (se comprimieron primero, luego se descomprimieron) \n")
(display "\nimgextra1: \n")
(decompress imgextra8)
(display "\nimgextra2: \n")
(decompress imgextra9)
(display "\nimgextra3: \n")
(decompress imgextra10)

(display "\n## FIN DE LAS PRUEBAS EXTRA. INICIO DEL  \"SCRIPT  BÁSICO DE PRUEBAS\"  \n\n")

;################################### SCRIPT DE PRUEBAS BASICO ################################
; El script original tiene varios errores de definición, que generan errores no relacionados al código
; implementado en el laboratorio.
; Estas lineas fueron comentadas y señaladas con:
; #### LINEA MAL DEFINIDA ###
; Ya que se decidió no cambiar el script de pruebas,
; se arreglaron los errores de paréntesis y se comentaron las lineas que dan error

;img1
;Creación de una imagen de 2 x 2 del tipo pixmap
(define img1 (image 2 2
                  (pixrgb-d 0 0 255 0 0 10)
                  (pixrgb-d 0 1 0 255 0 20)
                  (pixrgb-d 1 0 0 0 255 10)
                  (pixrgb-d 1 1 255 255 255  1)))
;img2
;Creación de una imagen de 2 x 2 del tipo bitmap
(define img2 (image 2 2
                  (pixbit-d 0 0 0 10)
                  (pixbit-d 0 1 1 20)
                  (pixbit-d 1 0 1 10)
                  (pixbit-d 1 1 0 255)))

(define img3 (imgRGB->imgHex img1))


;imprimir una representación string de la imagen
(display (image->string img1 pixrgb->string))

;output:
; #FF0000 #00FF00
; #0000FF #FFFFFF

;imprimir una representación string de la imagen
(display (image->string img2 pixbit->string))

;output:
;0 1
;1 0

(bitmap? img1) ; la respuesta debería ser #f
(bitmap? img2)  ; la respuesta debería ser #t
(bitmap? img3)  ; la respuesta debería ser #f

(pixmap? img1) ; la respuesta debería ser #t
(pixmap? img2)  ; la respuesta debería ser #f
(pixmap? img3)  ; la respuesta debería ser #f

(hexmap? img1) ; la respuesta debería ser #f
(hexmap? img2)  ; la respuesta debería ser #f
(hexmap? img3)  ; la respuesta debería ser #t

(compressed? img1) ; la respuesta debería ser #f
(compressed? img2) ; la respuesta debería ser #f
(compressed? img3) ; la respuesta debería ser #f

(flipH img1)
(flipH img2)
(flipH img3)

(flipV img1)
(flipV img2)
(flipV img3)

(define img4 (crop img1 0 0 0 0)) ; debería retornar una imágen con un pixel
(define img5 (crop img2 0 0 0 1)) ; debería retornar una imágen con dos pixeles
(define img6 (crop img1 0 1 1 1)) ; debería retornar una imágen con dos pixeles
(define img7 (crop img2 0 0 1 1)) ; debería retornar la misma imagen

(histogram img1)
(histogram img2)
(histogram img3)
(histogram img4)
(histogram img5)
(histogram img6)
(histogram img7)

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

(compressed? img8)  ; la respuesta debería ser #t
(compressed? img9)  ; la respuesta debería ser #t
(compressed? img10)  ; la respuesta debería ser #t
(compressed? img11)  ; la respuesta debería ser #t
(compressed? img12)  ; la respuesta debería ser #t
(compressed? img13)  ; la respuesta debería ser #t
(compressed? img14)  ; la respuesta debería ser #t

(define img15 (edit invertColorBit img2))
(define img16 (edit invertColorRGB img1))

;se asume que las funciones de ajuste de canal están implementadas. 
;Puede cambiarlas por otras en su script de pruebas si así lo prefiere 
(define img33 (edit (adjustChannel getR setR incCh) img1))
(define img34 (edit (adjustChannel getG setG incCh) img1))
(define img35 (edit (adjustChannel getB setB incCh) img1))

;imágenes no comprimidas
(display (image->string img1 pixrgb->string))
(display (image->string img2 pixbit->string))
(display (image->string img3 pixhex->string))
(display (image->string img4 pixrgb->string))
(display (image->string img5 pixbit->string))
(display (image->string img6 pixrgb->string))

; #### LINEA MAL DEFINIDA ###
;(display (image->string img7 pixrbit->string))

;imagenes comprimidas, podrían internamente descomprimirlas para convertir a string ;(opcional)
(display (image->string img8 pixrgb->string))
(display (image->string img9 pixbit->string))
(display (image->string img10 pixhex->string)) 
(display (image->string img11 pixrgb->string))
(display  (image->string img12 pixbit->string))
(display (image->string img13 pixrgb->string))
(display (image->string img14 pixbit->string))

;imágenes no comprimidas

; #### LINEA MAL DEFINIDA ###
;(display (image->string img15 pixrgb->string))
(display (image->string img16 pixrgb->string))

; #### LINEA MAL DEFINIDA ###
;(display (image->string img17 pixrgb->string))
(display (image->string img18 pixrgb->string))
(display (image->string img19 pixbit->string))
(display (image->string img20 pixhex->string))
(display (image->string img21 pixrgb->string))
(display (image->string img22 pixbit->string))
(display (image->string img23 pixrgb->string))
(display (image->string img24 pixbit->string))

(depthLayers img1)
(depthLayers img2)
(depthLayers img3)
(depthLayers img4)
(depthLayers img5)
(depthLayers img6)
(depthLayers img7)

(define img25 (decompress img8))
(define img26 (decompress img9))
(define img27 (decompress img10))
(define img28 (decompress img11))
(define img29 (decompress img12))
(define img30 (decompress img13))
(define img31 (decompress img14))

;las siguientes comparaciones deberían arrojar #t
(equal? img25 img1)
(equal? img26 img2)
(equal? img27 img3)
(equal? img28 img4)
(equal? img29 img5)
(equal? img30 img6)
(equal? img31 img7)

;las siguientes comparaciones deberían arrojar #f
(equal? img25 img2)
(equal? img26 img1)
