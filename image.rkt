#lang racket

(provide (all-defined-out))
(require "otras_funciones.rkt")
(require "pixel.rkt")

;;; TDA image:

; Entrada: 
; Salida: 
; Tipo de Recursión: 
; Algoritmo/Estrategia: 
; Descripción: 

;; constructor

; Entrada: tipo (String) X ancho (Entrada) X alto (Entrada) X Pixel X Pixel ...
; Salida: Image
; Descripción: Crea una imagen no comprimida, no se revisa si la cantidad de pixeles corresponde al área de la imagen
(define (image width height . pixels) (list "image" width height -1 pixels))


;; selectores
; Entrada: Image
; Salida: Ancho
; Descripción: Retorna el ancho de la imagen
(define (getImgW img) (second img))
; Entrada: Image
; Salida: Alto
; Descripción: Retorna el alto de la imagen
(define (getImgH img) (third img))
; Entrada: Image
; Salida: Valor de Compresión
; Descripción: Retorna el color de los pixeles eliminados por compress (valor de compresión)
(define (getImgComp img) (fourth img))
; Entrada: Image
; Salida: Lista
; Descripción: Retorna la lista de pixeles almacenada por la imagen
(define (getImgPixs img) (fifth img))


;; modificadores // no se usa "image" para retornar una imagen, ya que los datos de compresión se pierden
; Entrada: Image X Valor (Entero)
; Salida: Image
; Descripción: Retorna una imagen idéntica, con el ancho modificado
(define (setImgW img val) (list "image" val (getImgH img) (getImgComp img) (getImgPixs img)))
; Entrada: Image X Valor (Entero)
; Salida: Image
; Descripción: Retorna una imagen idéntica, con la altura modificada
(define (setImgH img val) (list "image" (getImgW img) val (getImgComp img) (getImgPixs img)))
; Entrada: Image X Valor (Entero | Lista de 3 Enteros | String)
; Salida: Image
; Descripción: Retorna una imagen idéntica, con el valor de compresión modificado
(define (setImgComp img val) (list "image" (getImgW img) (getImgH img) val (getImgPixs img)))
; Entrada: Image X Valor (Lista de Pixeles)
; Salida: Image
; Descripción: Retorna una imagen idéntica, con la lista de pixeles modificada
(define (setImgPixs img val) (list "image" (getImgW img) (getImgH img) (getImgComp img) val))
; Entrada: Image X Valor Ancho (Entero) X Valor Alto (Entero)
; Salida: Image
; Descripción: Retorna una imagen idéntica, con el ancho y el alto modificado
(define (setImgWH img valw valh) (list "image" valw valh (getImgComp img) (getImgPixs img)))


;; pertenencia
; Entrada: Imagen | Elemento basado en listas
; Salida: Booleano
; Descripción: Revisa si la lista de la entrada tiene en la primera posición la string "image" y que los siguientes elementos sean los valores de ancho y altura
(define (image? check) (and (equal? "image" (first check)) (number? (getImgW check)) (number? (getImgH check))))


;; otras funciones
; Entrada: Image
; Salida: Booleano
; Algoritmo/Estrategia: Se revisa si el valor de compresión nos puede decir el tipo de pixeles, sino se revisa cada pixel a fuerza bruta
; Descripción: Revisa si la imagen es del tipo bitmap
(define (bitmap? img) (and (image? img) (if (compressed? img)
                                            (number? (getImgComp img))
                                            (andmap (lambda (pix) (pixbit? pix)) (getImgPixs img)))))
; Entrada: Image
; Salida: Booleano
; Algoritmo/Estrategia: Se revisa si el valor de compresión nos puede decir el tipo de pixeles, sino se revisa cada pixel a fuerza bruta
; Descripción: Revisa si la imagen es del tipo pixmap
(define (pixmap? img) (and (image? img) (if (compressed? img)
                                            (list? (getImgComp img))
                                            (andmap (lambda (pix) (pixrgb? pix)) (getImgPixs img)))))
; Entrada: Image
; Salida: Booleano
; Algoritmo/Estrategia: Se revisa si el valor de compresión nos puede decir el tipo de pixeles, sino se revisa cada pixel a fuerza bruta
; Descripción: Revisa si la imagen es del tipo hexmap
(define (hexmap? img) (and (image? img) (if (compressed? img)
                                            (string? (getImgComp img))
                                            (andmap (lambda (pix) (pixhex? pix)) (getImgPixs img)))))
; Entrada: Image
; Salida: Booleano
; Descripción: Revisa si la imagen fue comprimida, -1 significa que no, cualquier otro valor significa que si
(define (compressed? img) (not (equal? -1 (getImgComp img))))


;; Otras funciones
; Entrada: Lista
; Salida: String
; Tipo de Recursión: Natural
; Algoritmo/Estrategia: Recorrido lineal de una lista, implementado recursivamente
; Descripción: Transforma una lista de strings en una sola string, ejemplo: '("ABC" "123" "456") -> "ABC123456"
(define (strAppendList lst) (if (empty? lst)
                                ""
                                (string-append (car lst) (strAppendList (cdr lst)))))
; Entrada: Image X Función
; Salida: Lista 
; Descripción: función envoltorio de 'recImg'
(define (recorrerImg img func) (recImg 0 0 (getImgW img) (getImgH img) func))
; Entrada: i (Entero) X j (Entero) X maxI (Entero) X maxJ (Entero) X Función
; Salida: Lista Vacía/null | Lista
; Tipo de Recursión: De cola
; Algoritmo/Estrategia: simula un ciclo for anidado, implementado recursivamente, aplicando una función usando los valores de la iteración como variables de entrada
; Descripción: funcion que permite recorrer y buscar todos los pixeles de una imagen, aplicando una funcion a cada uno, esta función fue creada para aplicar 'findPix' y realizar operaciones que requieren recorrer todos los pixeles de una imagen
(define (recImg i j maxI maxJ func) (if (equal? j maxJ)
                                        null
                                        (if (equal? i maxI)
                                            (recImg 0 (+ j 1) maxI maxJ func)
                                            (cons (func i j) (recImg (+ i 1) j maxI maxJ func)))))



;; información de imagen

; Entrada: Image
; Salida: Pixel
; Algoritmo/Estrategia: Buscar por toda la lista el primer elemento que cumple la condición 'and'
; Descripción: obtener el pix en la pos X Y de la imagen, uso: ((findPix img) x y))
(define (findPix img) (lambda (x y) (findf (lambda (pix) (and (equal? x (getPixPosX pix))
                                                              (equal? y (getPixPosY pix)))) (getImgPixs img))))

; Entrada: Image
; Salida: Lista de valores
; Descripción: retorna una lista con los valores de colores y la cantidad de veces que sale en la imagen, esta función es un envoltorio de histoRec
(define (histogram img) (cond
                          [(bitmap? img) (histoRec (sort (pixs->val (getImgPixs img)) <) (list))]
                          [(pixmap? img) (map (lambda (data) (list (hex->rgb (first data)) (second data)))
                                              (histoRec (sort (pixs->val (getImgPixs (imgRGB->imgHex img))) string<?) (list)))]
                          [(hexmap? img) (histoRec (sort (pixs->val (getImgPixs img)) string<?) (list)) ]))
; Entrada: Lista de valores de color X Lista de retorno | Lista Vacía/null
; Salida: Lista de valores de color X Lista de retorno | Lista Vacía/null  ||  Lista de retorno
; Tipo de Recursión: De cola
; Descripción: la función se encarga de contar las veces que se repite el primer color de la lista, guardar la información en la lista de retorno, y generar una lista nueva sin el color contado, repite hasta que la lista se vacie
(define (histoRec vals data) (if (empty? vals)
                                 data
                                 (histoRec (remove* (list (car vals)) vals) (append data (list (list (car vals) (count (lambda (val) (equal? (car vals) val)) vals)))))))

; Entrada: Lista (Salida de 'histogram')
; Salida: Entero | Lista de Enteros (R G B) | String (Valor de color)
; Descripción: funcion envoltorio de 'maxData'
(define (maxHistogramData data) (maxData data (list 0 0)))
; Entrada: Lista de elementos del tipo (valor X cantidad) X Lista
; Salida: Entero | Lista de Enteros (R G B) | String (Valor de color)
; Tipo de Recursión: De cola
; Algoritmo/Estrategia: recorrido lineal completo de la lista
; Descripción: recorre la lista que entrega histograma y retorna el valor de color que más se repite
(define (maxData data max) (if (empty? data)
                               (car max)
                               (if (> (second (first data)) (second max))
                                   (maxData (cdr data) (first data))
                                   (maxData (cdr data) max))))
; Entrada: Image
; Salida: Lista de pixeles
; Descripción: toma todos los pixeles de la imagen y devuelve la lista de valores
(define (pixs->val img) (map (lambda (pix) (getPixVal pix)) img))


;; transformación de imagenes
; Entrada: Image
; Salida: Image | String
; Descripción: Función envoltorio de 'flipPixV'
(define (flipV img) (if (image? img)
                        (setImgPixs img (flipPixV (getImgPixs img) null (- (getImgH img) 1)))
                        "No es una imagen"))
; Entrada: Lista de pixeles X Lista de pixeles dados vuelta X Altura (Entero)
; Salida: Lista de pixeles dados vuelta
; Tipo de Recursión: De cola
; Algoritmo/Estrategia: recorrido lineal de una lista
; Descripción: resta la altura a cada pixel de la imagen para dar la vuelta verticalmente
(define (flipPixV pixs flPixs imgH) (if (empty? pixs)
                                        flPixs
                                        (flipPixV (cdr pixs) (append flPixs (list (setPixPosY (car pixs) (abs  (- (getPixPosY (car pixs)) imgH))))) imgH)))
; Entrada: Image
; Salida: Image | String
; Descripción: Función envoltorio de 'flipPixH'
(define (flipH img) (if (image? img)
                        (setImgPixs img (flipPixH (getImgPixs img) null (- (getImgW img) 1)))
                        "No es una imagen"))
; Entrada: Lista de pixeles X Lista de pixeles dados vuelta X Ancho (Entero)
; Salida: Lista de pixeles dados vuelta
; Tipo de Recursión: De cola
; Algoritmo/Estrategia: recorrido lineal de una lista
; Descripción: resta el ancho a cada pixel de la imagen para dar la vuelta horizontalmente
(define (flipPixH pixs flPixs imgW) (if (empty? pixs)
                                        flPixs
                                        (flipPixH (cdr pixs) (append flPixs (list (setPixPosX (car pixs) (abs (- (getPixPosX (car pixs)) imgW))))) imgW)))


; Entrada: Image X x1 (Entero) X y1 (Entero) X x2 (Entero) X y2 (Entero)
; Salida: Image
; Descripción: retorna la imagen recortada, usando el punto x1,y1 como esquina de arriba a la izquierda, y el punto x2,y2 la esquina de abajo a la derecha como zona de recorte
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

; Entrada: pixmap
; Salida: hexmap
; Descripción: conversión pixmap a hexmap, transformando cada pixel individualmente
(define (imgRGB->imgHex imgrgb) (if (compressed? imgrgb)
                                    (imgRGB->imgHex (decompress imgrgb))
                                    (setImgPixs imgrgb (map (lambda (pix) (pixrgb->pixhex pix)) (getImgPixs imgrgb)))))

; Entrada: Image
; Salida: Image
; Descripción: rotación 90 grados a la derecha, o en sentido horariol. Se intercambia la coord. X e Y de cada pixel y se aplica 'flipH'
(define (rotate90 img) (flipH (setImgWH (setImgPixs img (map (lambda (pix) (swapPixXY pix)) (getImgPixs img))) (getImgH img) (getImgW img))))

; Entrada: Image
; Salida: Image
; Descripción: función envoltorio de 'subCompress'
(define (compress img) (subCompress img (maxHistogramData (histogram img))))
; Entrada: Image X (Entero | Lista de Enteros (R G B) | String (Valor de color))
; Salida: Image
; Descripción: Se crea una imagen nueva sin los pixeles del color ingresado en la entrada, y se asigna el valor de compresión de la imagen a ese valor para después poder descomprimir
(define (subCompress img val) (setImgComp (setImgPixs img (filter (lambda (pix) (not (equal? (getPixVal pix) val))) (getImgPixs img))) val))

; Entrada: Función X Image
; Salida: Image
; Descripción: función de orden superior para aplicar invertColorBit, invertColorRGB y adjustChannel
(define (edit func img) (setImgPixs img (map func (getImgPixs img))))

;; conversiones a string

; Entrada: Image X Función
; Salida: String
; Descripción: aplica a cada pixel la función de transformación a string y agrega el fin de linea y espacio según corresponda
(define (image->string img func) (if (compressed? img)
                                     (image->string (decompress img) func)
                                     (strAppendList (recorrerImg img (lambda (x y) (if (equal? x (- (getImgW img) 1))
                                                                                       (string-append (func ((findPix img) x y)) "\n")
                                                                                       (string-append (func ((findPix img) x y)) " ")))))))

;; depth layers
; Entrada: Image
; Salida: Lista de Imagenes
; Descripción: separa la imagen de entrada en múltiples imágenes según la profundidad de los pixeles
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

; Entrada: Image
; Salida: Lista
; Descripción: encuentra las profundidades existentes en la imagen y las retorna en una lista
(define (imgProfs img) (remove-duplicates (map (lambda (pix) (getPixDepth pix)) (getImgPixs img))))

; Entrada: Image
; Salida: Image
; Descripción: rellena los pixeles que no se encuentran en la imagen de entrada con el valor de compresión de la imagen, perdiendo por completo la profundidad
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


