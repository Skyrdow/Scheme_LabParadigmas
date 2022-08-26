#lang racket

(struct pixbit-d (x y bit depth))
(struct pixrgb-d (x y r g b depth))
(struct pixhex-d (x y hex depth))

(define (image width height . pixels) (list width height pixels))

;caddr accede a la lista "pixels"
(define (bitmap? img) (pixbit-d? (car (caddr img))))
(define (pixmap? img) (pixrgb-d? (car (caddr img))))
(define (hexmap? img) (pixhex-d? (car (caddr img))))

;tbd
(define (compressed? img) #f)

(define (flipH img) ())