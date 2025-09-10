#lang racket
(require sketching)
(provide draw-waves)

(define (draw-waves bg)
  (background bg)
  (stroke (color 255 140 0))
  (stroke-weight 8)
  (let ([t (/ (modulo (inexact->exact (floor (current-inexact-milliseconds))) 1000) 1000.0)])
    (for ([y (in-range 200 900 100)])
      (let ([amp 40]
            [freq 0.02])
        (for ([x (in-range 0 1920 10)])
          (let ([v (+ y (* amp (sin (+ (* freq x) (* 2 pi t)))) )])
            (point x v)))))))
