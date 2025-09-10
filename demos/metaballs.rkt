#lang racket
(require sketching)
(provide draw-metaballs)


(define metaballs-cols 64)
(define metaballs-rows 36)
(define metaballs-threshold 1.0)
(define metaballs-blobs-count 4)
(define metaballs-blob-radius-base 60)
(define metaballs-blob-radius-step 20)
(define metaballs-blob-offset-base 80)
(define metaballs-blob-offset-step 30)


(define (make-cell-w width) (/ width metaballs-cols))
(define (make-cell-h height) (/ height metaballs-rows))



(define (make-blobs width height)
  (for/list ([i (in-range metaballs-blobs-count)])
    (list
     (+ (* 0.2 width) (* i (/ width metaballs-blobs-count)))
     (+ (* 0.2 height) (* i (/ height metaballs-blobs-count)))
     (+ metaballs-blob-radius-base (* metaballs-blob-radius-step i))
     (+ metaballs-blob-offset-base (* metaballs-blob-offset-step i)))))




(define (update-blobs t width height)
  (for/list ([i (in-range metaballs-blobs-count)])
    (list
     (+ (/ width 2) (* (/ width 3) (sin (+ t (* i 1.1)))))
     (+ (/ height 2) (* (/ height 3) (cos (+ t (* i 1.3)))))
     (+ metaballs-blob-radius-base (* metaballs-blob-radius-step i))
     (+ metaballs-blob-offset-base (* metaballs-blob-offset-step i)))))

(define (field x y blobs)
  (for/sum ([b blobs])
    (let* ([bx (list-ref b 0)]
           [by (list-ref b 1)]
           [r (list-ref b 2)])
      (/ r (+ 1 (sqrt (+ (sqr (- x bx)) (sqr (- y by)))))))))



;; Accepts a background color argument

;; Accepts a background color argument, uses canvas size
(define (draw-metaballs bg)
  (background bg)
  (define w (width))
  (define h (height))
  (define t (/ (current-inexact-milliseconds) 1000.0))
  (define cell-w (make-cell-w w))
  (define cell-h (make-cell-h h))
  (define bs (update-blobs t w h))
  (ellipse-mode 'center)
  (no-stroke)
  (fill (color 255 140 0))
  (for* ([i (in-range metaballs-cols)] [j (in-range metaballs-rows)])
    (define x (+ (* i cell-w) (/ cell-w 2)))
    (define y (+ (* j cell-h) (/ cell-h 2)))
    (when (>= (field x y bs) metaballs-threshold)
      (ellipse x y cell-w cell-h))))
