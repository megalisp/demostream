
#lang racket
(require sketching)
(provide draw-spiral)

;; Accepts a background color argument
(define (draw-spiral bg)
  (background bg)
  (stroke (color 255 140 0))
  (stroke-weight 8)
  (no-fill)
  (define cx (/ 1920 2))
  (define cy (/ 1080 2))
  (define t (/ (modulo (inexact->exact (floor (current-inexact-milliseconds))) 10000) 10000.0))
  (let loop ([i 1]
             [prev-x (+ cx (* 10 0 (cos 0)))]
             [prev-y (+ cy (* 10 0 (sin 0)))] )
    (when (< i 2000)
      (let* ([theta (* 0.1 i)]
             [r (* 10 (+ 1 (* 0.5 (sin (+ theta (* 2 pi t))))))]
             [x (+ cx (* r theta (cos theta)))]
             [y (+ cy (* r theta (sin theta)))])
        (line prev-x prev-y x y)
        (loop (+ i 1) x y)))))
