
#lang racket
(require sketching)
(provide draw-starfield)

(define stars (for/list ([i 400]) (list (- (random 1920) (/ 1920 2)) (- (random 1080) (/ 1080 2)) (random 1 4))))
;; Accepts a background color argument
(define (draw-starfield bg)
  (background bg)
  (stroke (color 255 140 0))
  (stroke-weight 6)
  (define t (/ (modulo (inexact->exact (floor (current-inexact-milliseconds))) 10000) 10000.0))
  (for ([s stars])
    (let* ([x (list-ref s 0)] [y (list-ref s 1)] [z (list-ref s 2)]
           [sx (+ (/ 1920 2) (* x (/ 1 z) (* 1.5 (+ 1 t)) ))]
           [sy (+ (/ 1080 2) (* y (/ 1 z) (* 1.5 (+ 1 t)) ))])
      (point sx sy))))
