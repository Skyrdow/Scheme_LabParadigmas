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


;; transformación de imagenes
(define (flipV img) (if (image? img)
                               (setImgPixs img (flipPixV (getImgPixs img) (list) (- (getImgH img) 1)))
                               (display "No es una imagen")))

(define (flipH img) (if (image? img)
                               (setImgPixs img (flipPixH (getImgPixs img) (list) (- (getImgW img) 1)))
                               (display "No es una imagen")))


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

(define (incCh val) (+ 1 val))
(define (decCh val) (- val 1))

;; conversiones a string
; obtener pix en la pos X Y
(define (findPix img x y) (findf (lambda (pix) (and (equal? x (getPixPosX pix)) (equal? y (getPixPosY pix)))) (getImgPixs img)))
; preguntar si existe pix en la pos X Y borrar [let]
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
                                     (strAppendList (for/list ([i (range 0 (getImgH img))])
                                                      (for/list ([j (range 0 (getImgW img))])
                                                        (if (equal? j (- (getImgW img) 1))
                                                            (string-append (func (findPix img j i)) "\n")
                                                            (string-append (func (findPix img j i)) " ")))))))



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
                                                                        (if (pixOnXY? img j i)
                                                                            (findPix img j i)
                                                                            (pixrgb-d j i (first (getImgComp img)) (second (getImgComp img)) (third (getImgComp img)) 0))))]
                              
                                       [(hexmap? img) (setImgPixs img (for*/list ([i (inclusive-range 0 (- (getImgH img) 1))]
                                                                                  [j (inclusive-range 0 (- (getImgW img) 1))])
                                                                        (if (pixOnXY? img j i)
                                                                            (findPix img j i)
                                                                            (pixhex-d j i (getImgComp img) 0))))])
                                     -1))
