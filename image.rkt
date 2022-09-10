#lang racket

(provide (all-defined-out))
(require "otras_funciones.rkt")
(require "pixel.rkt")

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
(define (image? check) (and (equal? "image" (first check)) (number? (getImgW check)) (number? (getImgH check))))


;; otras funciones
; tipo de imagen
(define (bitmap? img) (and (image? img) (if (compressed? img)
                                            (number? (getImgComp img))
                                            (andmap (lambda (pix) (pixbit? pix)) (getImgPixs img)))))
(define (pixmap? img) (and (image? img) (if (compressed? img)
                                            (list? (getImgComp img))
                                            (andmap (lambda (pix) (pixrgb? pix)) (getImgPixs img)))))
(define (hexmap? img) (and (image? img) (if (compressed? img)
                                            (string? (getImgComp img))
                                            (andmap (lambda (pix) (pixhex? pix)) (getImgPixs img)))))
(define (compressed? img) (not (equal? -1 (getImgComp img))))




; información de imagen


; obtener pix en la pos X Y
(define (findPix img) (lambda (x y) (findf (lambda (pix) (and (equal? x (getPixPosX pix))
                                                              (equal? y (getPixPosY pix)))) (getImgPixs img))))


; histograma
(define (histogram img) (cond
                          [(bitmap? img) (histoRec (sort (pixs->val (getImgPixs img)) <) (list))]
                          [(pixmap? img) (map (lambda (data) (list (hex->rgb (first data)) (second data)))
                                              (histoRec (sort (pixs->val (getImgPixs (imgRGB->imgHex img))) string<?) (list)))]
                          [(hexmap? img) (histoRec (sort (pixs->val (getImgPixs img)) string<?) (list)) ]))
; funcion suplementaria
(define (maxHistogramData data) (maxData data (list 0 0)))
(define (maxData data max) (if (empty? data)
                               (car max)
                               (if (> (second (first data)) (second max))
                                   (maxData (cdr data) (first data))
                                   (maxData (cdr data) max))))
; toma la lista de pixeles para recorrer recursivamente y data (valor cantidad)
(define (histoRec vals data) (if (empty? vals)
                                 data
                                 (histoRec (remove* (list (car vals)) vals) (append data (list (list (car vals) (count (lambda (val) (equal? (car vals) val)) vals)))))))
; toma todos los pixeles y devuelve la lista de valores
(define (pixs->val img) (map (lambda (pix) (getPixVal pix)) img))



;; transformación de imagenes
(define (flipV img) (if (image? img)
                        (setImgPixs img (flipPixV (getImgPixs img) (list) (- (getImgH img) 1)))
                        "No es una imagen"))

(define (flipH img) (if (image? img)
                        (setImgPixs img (flipPixH (getImgPixs img) (list) (- (getImgW img) 1)))
                        "No es una imagen"))
; funcion recursiva de apoyo a flip
(define (flipPixV pixs flPixs imgH) (if (empty? pixs)
                                        flPixs
                                        (flipPixV (cdr pixs) (append flPixs (list (setPixPosY (car pixs) (abs  (- (getPixPosY (car pixs)) imgH))))) imgH)))
; funcion recursiva de apoyo
(define (flipPixH pixs flPixs imgW) (if (empty? pixs)
                                        flPixs
                                        (flipPixH (cdr pixs) (append flPixs (list (setPixPosX (car pixs) (abs (- (getPixPosX (car pixs)) imgW))))) imgW)))


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
                                         (+ (- x2 x1) 1) (+ (- y2 y1) 1)))

; conversión rgb a hex
(define (imgRGB->imgHex imgrgb) (setImgPixs imgrgb (map (lambda (pix) (pixrgb->pixhex pix)) (getImgPixs imgrgb))))

; rotación 90 grados a la derecha, o en sentido horario
(define (rotate90 img) (flipH (setImgWH (setImgPixs img (map (lambda (pix) (swapPixXY pix)) (getImgPixs img))) (getImgH img) (getImgW img))))

; compress
(define (compress img) (subCompress img (maxHistogramData (histogram img))))
; wrapper
(define (subCompress img data) (setImgComp (setImgPixs img (filter (lambda (pix) (not (equal? (getPixVal pix) data))) (getImgPixs img))) data))



;; edit superfunction
(define (edit func img) (setImgPixs img (map func (getImgPixs img))))

; invColorBit
(define (invertColorBit pix) (setPixVal pix (- 1 (getPixVal pix))))
; invColorRGB
(define (invertColorRGB pix) (setPixVal pix (list (- 255 (getR pix)) (- 255 (getG pix)) (- 255 (getB pix)))))
; adj channel
(define adjustChannel (lambda (f1 f2 f3) (lambda (pix) (f2 pix (f3 (f1 pix))))))

(define (incCh val) (+ 1 val))
(define (decCh val) (- val 1))

;; conversiones a string
; preguntar si existe pix en la pos X Y borrar [let]
(define (pixOnXY? img x y) (if (not (false? ((findPix img) x y)))
                               #t
                               #f))
; sstring
(define (strAppendList lst) (if (empty? lst)
                                ""
                                (string-append (car lst) (strAppendList (cdr lst)))))

; funcion que permite recorrer y buscar todos los pixeles de una imagen, aplicando una funcion a cada uno
(define (recorrerImg img func) (recImg 0 0 (getImgW img) (getImgH img) func))
(define (recImg i j maxI maxJ func) (if (equal? j maxJ)
                                        null
                                        (if (equal? i maxI)
                                            (recImg 0 (+ j 1) maxI maxJ func)
                                            (cons (func i j) (recImg (+ i 1) j maxI maxJ func)))))

; image->string
(define (image->string img func) (if (compressed? img)
                                     (image->string (decompress img) func)
                                     (strAppendList (recorrerImg img (lambda (x y) (if (equal? x (- (getImgW img) 1))
                                                                                       (string-append (func ((findPix img) x y)) "\n")
                                                                                       (string-append (func ((findPix img) x y)) " ")))))))

;; depth layers
(define (depthLayers img) (cond
                            [(bitmap? img) (map
                                             (lambda (iDepth) (setImgPixs img (map (lambda (pix) (if (equal? iDepth (getPixDepth pix))
                                                                                    pix
                                                                                    (setPixVal pix 1)))
                                                                  (getImgPixs img)))) (imgProfs img))]

                            [(pixmap? img) (map
                                             (lambda (iDepth) (setImgPixs img (map (lambda (pix) (if (equal? iDepth (getPixDepth pix))
                                                                                    pix
                                                                                    (setPixVal pix '(255 255 255))))
                                                                  (getImgPixs img)))) (imgProfs img))]
                            [(hexmap? img) (map
                                             (lambda (iDepth) (setImgPixs img (map (lambda (pix) (if (equal? iDepth (getPixDepth pix))
                                                                                    pix
                                                                                    (setPixVal pix "#FFFFFF")))
                                                                  (getImgPixs img)))) (imgProfs img))]))

; encontrar las profundidades existentes en la imagen
(define (imgProfs img) (remove-duplicates (map (lambda (pix) (getPixDepth pix)) (getImgPixs img))))

;; decompress
(define (decompress img) (setImgComp (cond
                                       [(bitmap? img) (setImgPixs img (recorrerImg img (lambda (x y) (let ([pixOnXY ((findPix img) x y)])
                                                                                                       (if (false? pixOnXY)
                                                                                                           (pixbit-d x y (getImgComp img) 0)
                                                                                                           pixOnXY)))))]
                            
                                       [(pixmap? img) (setImgPixs img (recorrerImg img (lambda (x y) (let ([pixOnXY ((findPix img) x y)])
                                                                                                       (if (false? pixOnXY)
                                                                                                           (pixrgb-d x y (first (getImgComp img)) (second (getImgComp img)) (third (getImgComp img)) 0)
                                                                                                           pixOnXY)))))]
                              
                                       [(hexmap? img) (setImgPixs img (recorrerImg img (lambda (x y) (let ([pixOnXY ((findPix img) x y)])
                                                                                                       (if (false? pixOnXY)
                                                                                                           (pixhex-d x y (getImgComp img) 0)
                                                                                                           pixOnXY)))))])
                                     -1))


