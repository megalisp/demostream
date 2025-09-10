#lang racket
(require sketching)
(provide draw-lissajous)

(define lissajous-a 3)
(define lissajous-b 2)
(define lissajous-d (/ 1920 2))
(define lissajous-e (/ 1080 2))
(define lissajous-max 1000)
(define lissajous-size 400)
(define lissajous-weight 8)


(define (draw-lissajous bg)
  (background bg)
  (stroke (color 255 140 0))
  (stroke-weight lissajous-weight)
  (no-fill)
  (define t (/ (modulo (inexact->exact (floor (current-inexact-milliseconds))) 10000) 10000.0))
  (let loop ([i 1]
             [prev-x (+ lissajous-d (* lissajous-size (sin (+ (* lissajous-a 0) (* 2 pi t)))))]
             [prev-y (+ lissajous-e (* lissajous-size (sin (+ (* lissajous-b 0) (* 2 pi t)))))] )
    (when (< i lissajous-max)
      (let* ([theta (* 2 pi (/ i lissajous-max))]
             [x (+ lissajous-d (* lissajous-size (sin (+ (* lissajous-a theta) (* 2 pi t)))))]
             [y (+ lissajous-e (* lissajous-size (sin (+ (* lissajous-b theta) (* 2 pi t)))))] )
        (line prev-x prev-y x y)
        (loop (+ i 1) x y)))))
