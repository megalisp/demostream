#lang racket
(require sketching)

(define dvd-width 160)
(define dvd-height 80)

(define state (box #f))

(define (reset-dvd)
  (set-box! state
    (vector
      (+ 50 (random (- (width) 200)))
      (+ 50 (random (- (height) 200)))
  (* 1.5 (if (zero? (random 2)) 1 -1) (+ 5 (random 4))) ; 1.5x faster dx
  (* 1.5 (if (zero? (random 2)) 1 -1) (+ 5 (random 4))) ; 1.5x faster dy
  (random-color))))

;; Use the public 'color' function instead of 'make-color'
(define (random-color)
  (color (random 256) (random 256) (random 256)))

(define (draw-dvd)
  (when (not (unbox state)) (reset-dvd))
  (let* ([s (unbox state)]
         [x (vector-ref s 0)]
         [y (vector-ref s 1)]
         [dx (vector-ref s 2)]
         [dy (vector-ref s 3)]
         [col (vector-ref s 4)]
         [nx (+ x dx)]
         [ny (+ y dy)]
         [ndx dx]
         [ndy dy]
         [ncol col]
         [text-height 48] ; matches text-size
         [total-top-offset 10] ; vertical offset above rect for text
         [box-top (- ny text-height total-top-offset)]
         [box-bottom (+ ny dvd-height)]
         [box-left nx]
         [box-right (+ nx dvd-width)])
    (when (or (< box-left 0) (> box-right (width)))
      (set! ndx (- ndx)))
    (when (or (< box-top 0) (> box-bottom (height)))
      (set! ndy (- ndy)))
    (set-box! state (vector (+ x ndx) (+ y ndy) ndx ndy ncol))
    (background (color 0 0 0))
    (fill (color 255 140 0)) ; orange
    (rect (+ x ndx) (+ y ndy) dvd-width dvd-height)
    (fill (color 255 255 255))
    (text-size 48)
    (text-align 'center 'bottom)
    (text "DVD" (+ x ndx (/ dvd-width 2)) (+ y ndy -10))))

(provide draw-dvd)
