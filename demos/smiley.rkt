
#lang racket
(require sketching)


(define (draw-smiley-face x y r)
  (fill 255 140 0) ; orange face
  (ellipse x y r r) ; face
  (fill 0) ; black eyes
  (ellipse (- x (/ r 5)) (- y (/ r 5)) (/ r 6) (/ r 6)) ; left eye
  (ellipse (+ x (/ r 5)) (- y (/ r 5)) (/ r 6) (/ r 6)) ; right eye
  (no-fill)
  (stroke 0)
  (stroke-weight 30)
  (arc x (- y (* 0.08 r)) (/ r 2) (/ r 3) 0 pi 'open)
  (stroke-weight 4)) ; smile (upwards, much thicker, 20% lower)


(define angle 0)



(define (draw-smiley bg)
  (background bg)
  (translate (/ (width) 2) (/ (height) 2))
  (rotate angle)
  (set! angle (+ angle 0.03))
  (draw-smiley-face 0 0 (* 0.7 (min (width) (height)))))


(provide draw-smiley)
