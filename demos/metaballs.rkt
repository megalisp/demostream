#lang racket
(require sketching)
(provide draw-metaballs)

(define cols 64)
(define rows 36)

(define (make-cell-w width) (/ width cols))
(define (make-cell-h height) (/ height rows))
(define threshold 1.0)
(define blobs-count 4)


(define (make-blobs width height)
  (for/list ([i (in-range blobs-count)])
    (list
     (+ (* 0.2 width) (* i (/ width blobs-count)))
     (+ (* 0.2 height) (* i (/ height blobs-count)))
     (+ 60 (* 20 i))
     (+ 80 (* 30 i)))))


(define (update-blobs t width height)
  (for/list ([i (in-range blobs-count)])
    (list
     (+ (/ width 2) (* (/ width 3) (sin (+ t (* i 1.1)))))
     (+ (/ height 2) (* (/ height 3) (cos (+ t (* i 1.3)))))
     (+ 60 (* 20 i))
     (+ 80 (* 30 i)))))

(define (field x y blobs)
  (for/sum ([b blobs])
    (let* ([bx (list-ref b 0)]
           [by (list-ref b 1)]
           [r (list-ref b 2)])
      (/ r (+ 1 (sqrt (+ (sqr (- x bx)) (sqr (- y by)))))))))



;; Accepts a background color argument
(define (draw-metaballs width height bg)
  (background bg)
  (define t (/ (current-inexact-milliseconds) 1000.0))
  (define cell-w (make-cell-w width))
  (define cell-h (make-cell-h height))
  (define bs (update-blobs t width height))
  (ellipse-mode 'center)
  (no-stroke)
  (fill (color 255 140 0))
  (for* ([i (in-range cols)] [j (in-range rows)])
    (define x (+ (* i cell-w) (/ cell-w 2)))
    (define y (+ (* j cell-h) (/ cell-h 2)))
    (when (>= (field x y bs) threshold)
      (ellipse x y cell-w cell-h))))
