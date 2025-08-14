
#lang racket
(require sketching)
(provide draw-lissajous)

;; Accepts a background color argument
(define (draw-lissajous bg)
  (background bg)
  (stroke (color 255 140 0))
  (stroke-weight 8)
  (no-fill)
  (define t (/ (modulo (inexact->exact (floor (current-inexact-milliseconds))) 10000) 10000.0))
  (define a 3)
  (define b 2)
  (define d (/ 1920 2))
  (define e (/ 1080 2))
  (let loop ([i 1]
             [prev-x (+ d (* 400 (sin (+ (* a 0) (* 2 pi t)))))]
             [prev-y (+ e (* 400 (sin (+ (* b 0) (* 2 pi t)))))] )
    (when (< i 1000)
      (let* ([theta (* 2 pi (/ i 1000.0))]
             [x (+ d (* 400 (sin (+ (* a theta) (* 2 pi t)))))]
             [y (+ e (* 400 (sin (+ (* b theta) (* 2 pi t)))))] )
        (line prev-x prev-y x y)
        (loop (+ i 1) x y)))))
