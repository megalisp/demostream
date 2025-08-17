
#lang racket
(require sketching)
(provide draw-spiral)

(define spiral-max 2000)
(define spiral-size 10)
(define spiral-weight 8)

(define (draw-spiral bg)
  (background bg)
  (stroke (color 255 140 0))
  (stroke-weight spiral-weight)
  (no-fill)
  (define spiral-cx (/ 1920 2))
  (define spiral-cy (/ 1080 2))
  (define t (/ (modulo (inexact->exact (floor (current-inexact-milliseconds))) 10000) 10000.0))
  (let loop ([i 1]
             [prev-x (+ spiral-cx (* spiral-size 0 (cos 0)))]
             [prev-y (+ spiral-cy (* spiral-size 0 (sin 0)))] )
    (when (< i spiral-max)
      (let* ([theta (* 0.1 i)]
             [r (* spiral-size (+ 1 (* 0.5 (sin (+ theta (* 2 pi t))))))]
             [x (+ spiral-cx (* r theta (cos theta)))]
             [y (+ spiral-cy (* r theta (sin theta)))])
        (line prev-x prev-y x y)
        (loop (+ i 1) x y)))))
