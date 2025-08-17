
#lang racket
(require sketching)
(provide draw-starfield)

(define starfield-count 400)
(define starfield-min-z 1)
(define starfield-max-z 4)
(define starfield-width 1920)
(define starfield-height 1080)
(define starfield-stars (for/list ([i starfield-count]) (list (- (random starfield-width) (/ starfield-width 2)) (- (random starfield-height) (/ starfield-height 2)) (random starfield-min-z starfield-max-z))))


(define (draw-starfield bg)
  (background bg)
  (stroke (color 255 140 0))
  (stroke-weight 6)
  (define t (/ (modulo (inexact->exact (floor (current-inexact-milliseconds))) 10000) 10000.0))
  (for ([s starfield-stars])
    (let* ([x (list-ref s 0)] [y (list-ref s 1)] [z (list-ref s 2)]
           [sx (+ (/ starfield-width 2) (* x (/ 1 z) (* 1.5 (+ 1 t)) ))]
           [sy (+ (/ starfield-height 2) (* y (/ 1 z) (* 1.5 (+ 1 t)) ))])
      (point sx sy))))
