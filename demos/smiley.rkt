
#lang racket
(require sketching)
(provide draw-smiley)


(define smiley-angle 0)
(define smiley-size-mult 0.7)
(define smiley-weight 30)
(define smiley-eye-ratio 5)
(define smiley-mouth-ratio 2)
(define smiley-mouth-thickness 4)


(define (draw-smiley-face x y r)
  (fill 255 140 0) ; orange face
  (ellipse x y r r) ; face
  (fill 0) ; black eyes
  (ellipse (- x (/ r smiley-eye-ratio)) (- y (/ r smiley-eye-ratio)) (/ r 6) (/ r 6)) ; left eye
  (ellipse (+ x (/ r smiley-eye-ratio)) (- y (/ r smiley-eye-ratio)) (/ r 6) (/ r 6)) ; right eye
  (no-fill)
  (stroke 0)
  (stroke-weight smiley-weight)
  (arc x (- y (* 0.08 r)) (/ r smiley-mouth-ratio) (/ r 3) 0 pi 'open)
  (stroke-weight smiley-mouth-thickness)) ; smile (upwards, much thicker, 20% lower)


(define (draw-smiley bg)
  (background bg)
  (translate (/ (width) 2) (/ (height) 2))
  (rotate smiley-angle)
  (set! smiley-angle (+ smiley-angle 0.03))
  (draw-smiley-face 0 0 (* smiley-size-mult (min (width) (height)))))


