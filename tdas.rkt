#lang racket

(struct pixbit-d (x y bit depth))
(struct pixrgb-d (x y r g b depth))
(struct pixhex-d (x y hex depth))

(struct image (width height pixels))

;falta completar image para sacar los parametros de pixels sin listas
(define (bitmap? img)  (first (first (image-pixels img))))