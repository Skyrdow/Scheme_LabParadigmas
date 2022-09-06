#lang racket

(provide (all-defined-out))
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
(define (setPixXY pix x y) (list (getPixType pix) x y (getPixVal pix) (getPixDepth pix)))

;; otras funciones
; función generica que compara el tipo de un pixel
(define (compType type pix) (equal? type (getPixType pix)))
; intercambia coordenadas x e y de un pixel
(define (swapPixXY pix) (setPixXY  pix (getPixPosY pix) (getPixPosX pix)))

  
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
(define (setImgWH img valw valh) (list "image" valw valh (getImgComp img) (getImgPixs img)))


;; pertenencia
(define (image? check) (equal? "image" (first check)))


;; otras funciones
; tipo de imagen
(define (bitmap? img) (and (image? img) (if (compressed? img)
                                            (number? (getImgComp img))
                                            (pixbit? (first (getImgPixs img))))))
(define (pixmap? img) (and (image? img) (if (compressed? img)
                                            (list? (getImgComp img))
                                            (pixrgb? (first (getImgPixs img))))))
(define (hexmap? img) (and (image? img) (if (compressed? img)
                                            (string? (getImgComp img))
                                            (pixhex? (first (getImgPixs img))))))
(define (compressed? img) (not (equal? -1 (getImgComp img))))

; transforma numeros en base 10 a hex dejando un 0 para valores entre 0 y 16
(define (int->hex val) (if (< 16 val)
                           (number->string val 16)
                           (string-append "0" (number->string val 16))))
; transforma valores (r g b) de un pixrgb a pixel hexadecimal "#RRGGBB"
(define (pixrgb->pixhex pix) (setPixType (setPixVal pix (string-upcase (string-append "#" (int->hex (getPixR pix)) (int->hex (getPixG pix)) (int->hex (getPixB pix))))) "pixhex-d"))
; de hex a rgb
(define (hex->rgb hex) (list (hex->int (substring hex 1 3)) (hex->int (substring hex 3 5)) (hex->int (substring hex 5 7))))
; string hex a int
(define (hex->int str) (string->number str 16))

; información de imagen
; histograma
(define (histogram img) (cond
                          [(bitmap? img) (histoRec (sort (pixs->val (getImgPixs img)) <) (list))]
                          [(pixmap? img) (map (lambda (data) (list (hex->rgb (first data)) (second data)))
                                              (histoRec (sort (pixs->val (getImgPixs (imgRGB->imgHex img))) string<?) (list)))]
                          [(hexmap? img) (histoRec (sort (pixs->val (getImgPixs img)) string<?) (list)) ]))
; toma la lista de pixeles para recorrer recursivamente y data (valor cantidad)
(define (histoRec vals data) (if (empty? vals)
                                 data
                                 (histoRec (remove* (list (car vals)) vals) (append data (list (list (car vals) (count (lambda (val) (equal? (car vals) val)) vals)))))))
; toma todos los pixeles y devuelve la lista de valores
(define (pixs->val img) (map (lambda (pix) (getPixVal pix)) img))

;; transformación de imagenes
(define (flipV img) (if (image? img)
                               (setImgPixs img (flipPixV (getImgPixs img) (list) (getImgW img)))
                               (display "No es una imagen")))
; funcion recursiva de apoyo
(define (flipPixV pixs flPixs imgW) (if (empty? pixs)
                                        flPixs
                                        (flipPixV (cdr pixs) (append flPixs (list (setPixPosX (car pixs) (abs (+ 1 (- (getPixPosX (car pixs)) imgW)))))) imgW)))

(define (flipH img) (if (image? img)
                               (setImgPixs img (flipPixH (getImgPixs img) (list) (getImgH img)))
                               (display "No es una imagen")))
; funcion recursiva de apoyo
(define (flipPixH pixs flPixs imgH) (if (empty? pixs)
                                        flPixs
                                        (flipPixH (cdr pixs) (append flPixs (list (setPixPosY (car pixs) (abs (+ 1 (- (getPixPosY (car pixs)) imgH)))))) imgH)))

; punto 1 esquina de arriba a la izquierda, punto 2 esquina de abajo a la derecha
(define (crop img x1 y1 x2 y2) (setImgWH (setImgPixs img
                                                     (map (lambda (pix) (setPixXY pix (- (getPixPosX pix) x1) (- (getPixPosY pix) y1)))
                                                          (filter (lambda (pix) (if (and (<= x1 (getPixPosX pix))
                                                                                      (>= x2 (getPixPosX pix))
                                                                                      (<= y1 (getPixPosY pix))
                                                                                      (>= y2 (getPixPosY pix)))
                                                                                 #t
                                                                                 #f))
                                                                  (getImgPixs img))))
                                         (- (getImgW img) y1) (-  (getImgH img) x1)))

; conversión rgb a hex
(define (imgRGB->imgHex imgrgb) (setImgPixs imgrgb (map (lambda (pix) (pixrgb->pixhex pix)) (getImgPixs imgrgb))))

; rotación 90 grados a la derecha, o en sentido horario
(define (rotate90 img) (flipH (setImgWH (setImgPixs img (map (lambda (pix) (swapPixXY pix)) (getImgPixs img))) (getImgH img) (getImgW img))))

; compress
(define (compress img) (subCompress img (maxHistogramData (histogram img))))
; wrapper
(define (subCompress img data) (setImgComp (setImgPixs img (filter (lambda (pix) (not (equal? (getPixVal pix) data))) (getImgPixs img))) data))

; funcion suplementaria
(define (maxHistogramData data) (maxData data (list 0 0)))
(define (maxData data max) (if (empty? data)
                               (car max)
                               (if (> (second (first data)) (second max))
                                   (maxData (cdr data) (first data))
                                   (maxData (cdr data) max))))

;; edit superfunction
(define (edit func img) (setImgPixs img (map func (getImgPixs img))))

; invColorBit
(define (invertColorBit pix) (setPixVal pix (- 1 (getPixVal pix))))
; invColorRGB
(define (invertColorRGB pix) (setPixVal pix (list (- 255 (getPixR pix)) (- 255 (getPixG pix)) (- 255 (getPixB pix)))))
; adj channel
(define adjustChannel (lambda (f1 f2 f3) (lambda (pix) (f2 pix (f3 (f1 pix))))))

(define (getR pix) (getPixR pix))
(define (setR pix valR) (setPixVal pix (list valR (getPixG pix) (getPixB pix))))
(define (getG pix) (getPixG pix))
(define (setG pix valG) (setPixVal pix (list (getPixR pix) valG (getPixB pix))))
(define (getB pix) (getPixB pix))
(define (setB pix valB) (setPixVal pix (list (getPixR pix) (getPixG pix) valB)))

(define (incCh val) (+ 1 val))
(define (decCh val) (- val 1))

;; conversiones a string
; obtener pix en la pos X Y
(define (findPix img x y) (findf (lambda (pix) (and (equal? x (getPixPosX pix)) (equal? y (getPixPosY pix)))) (getImgPixs img)))
; preguntar si existe pix en la pos X Y
(define (pixOnXY? img x y) (if (not (false? (findPix img x y)))
                               #t
                               #f))
; sstring
(define (strAppendList lst) (if (empty? lst)
                                ""
                                (if (list? (car lst))
                                    (string-append (strAppendList (car lst)) (strAppendList (cdr lst)))
                                    (string-append (car lst) (strAppendList (cdr lst))))))
; image->string
(define (image->string img func) (if (compressed? img)
                                     (image->string (decompress img) func)
                                     (strAppendList (for/list ([i (inclusive-range 0 (- (getImgH img) 1))])
                                                      (for/list ([j (inclusive-range 0 (- (getImgW img) 1))])
                                                        (if (equal? j (- (getImgW img) 1))
                                                            (string-append (func (findPix img i j)) "\n")
                                                            (string-append (func (findPix img i j)) " ")))))))
; pixbit->string
(define (pixbit->string pix) (if (false? pix)
                                 "#------"
                                 (if (equal? 0 (getBit pix))
                                     "#000000"
                                     "#FFFFFF")))
; pixrgb->string
(define (pixrgb->string pix) (if (false? pix)
                                 "#------"
                                 (pixhex->string (pixrgb->pixhex pix))))
; pixhex->string
(define (pixhex->string pix) (if (false? pix)
                                 "#------"
                                 (getPixVal pix)))

;; depth layers
(define (depthLayers img) (cond
                            [(bitmap? img) (for/list ([i (imgProfs img)])
                                             (setImgPixs img (map (lambda (pix) (if (equal? i (getPixDepth pix))
                                                                                    pix
                                                                                    (setPixVal pix 0)))
                                                                  (getImgPixs img))))]
                            [(pixmap? img) (for/list ([i (imgProfs img)])
                                             (setImgPixs img (map (lambda (pix) (if (equal? i (getPixDepth pix))
                                                                                    pix
                                                                                    (setPixVal pix '(255 255 255))))
                                                                  (getImgPixs img))))]
                            [(hexmap? img) (for/list ([i (imgProfs img)])
                                             (setImgPixs img (map (lambda (pix) (if (equal? i (getPixDepth pix))
                                                                                    pix
                                                                                    (setPixVal pix "#FFFFFF")))
                                                                  (getImgPixs img))))]))

; encontrar las profundidades existentes en la imagen
(define (imgProfs img) (remove-duplicates (map (lambda (pix) (getPixDepth pix)) (getImgPixs img))))

;; decompress
(define (decompress img) (setImgComp (cond
                                       [(bitmap? img) (setImgPixs img (for*/list ([i (inclusive-range 0 (- (getImgH img) 1))]
                                                                                  [j (inclusive-range 0 (- (getImgW img) 1))])
                                                                               (if (pixOnXY? img i j)
                                                                                   (findPix img i j)
                                                                                   (pixbit-d i j (getImgComp img) 0))))]
                            
                                       [(pixmap? img) (setImgPixs img (for*/list ([i (inclusive-range 0 (- (getImgH img) 1))]
                                                                             [j (inclusive-range 0 (- (getImgW img) 1))])
                                                                               (if (pixOnXY? img i j)
                                                                                   (findPix img i j)
                                                                                   (pixrgb-d i j (first (getImgComp img)) (second (getImgComp img)) (third (getImgComp img)) 0))))]
                              
                                       [(hexmap? img) (setImgPixs img (for*/list ([i (inclusive-range 0 (- (getImgH img) 1))]
                                                                                  [j (inclusive-range 0 (- (getImgW img) 1))])
                                                                               (if (pixOnXY? img i j)
                                                                                   (findPix img i j)
                                                                                   (pixhex-d i j (getImgComp img) 0))))])
                                     -1))
