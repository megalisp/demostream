
#lang racket
(require sketching)
(provide draw-tunnel)

(define tunnel-width 1920)
(define tunnel-height 1080)
(define tunnel-n-rings 18)
(define tunnel-n-points 32)
(define tunnel-length 2000)
(define tunnel-radius 400)
(define tunnel-speed 30)

;; Real modulo for positive modulus
(define (real-modulo x m)
  (let ([r (- x (* m (floor (/ x m))))])
    (if (< r 0) (+ r m) r)))


(define (project x y z)
  (let* ([fov 800]
         [scale (/ fov z)])
    (list (+ (/ tunnel-width 2) (* x scale))
          (+ (/ tunnel-height 2) (* y scale)))))

;; Accepts a background color argument

(define (draw-tunnel bg)
  (background bg)
  (define t (/ (current-inexact-milliseconds) 1000.0))
  (define dz tunnel-speed)
  (define rings
    (for/list ([i (in-range tunnel-n-rings)])
      (let* ((z (+ 200 (* (/ i tunnel-n-rings) tunnel-length) (real-modulo (* t dz) tunnel-length)))
             (z (real-modulo z tunnel-length))
             (z (+ 1 z))) ; avoid div by zero
        (for/list ([j (in-range tunnel-n-points)])
          (let* ((angle (* 2 pi (/ j tunnel-n-points)))
                 (x (* tunnel-radius (cos angle)))
                 (y (* tunnel-radius (sin angle))))
            (project x y z))))))
  (stroke (color 255 140 0))
  (stroke-weight 6)
  ;; Draw rings
  (for ([ring rings])
    (for ([j (in-range tunnel-n-points)])
      (define p1 (list-ref ring j))
      (define p2 (list-ref ring (modulo (+ j 1) tunnel-n-points)))
      (line (list-ref p1 0) (list-ref p1 1) (list-ref p2 0) (list-ref p2 1))))
  ;; Draw lines between rings
  (for ([i (in-range (- tunnel-n-rings 1))])
    (define ring1 (list-ref rings i))
    (define ring2 (list-ref rings (+ i 1)))
    (for ([j (in-range tunnel-n-points)])
      (define p1 (list-ref ring1 j))
      (define p2 (list-ref ring2 j))
      (line (list-ref p1 0) (list-ref p1 1) (list-ref p2 0) (list-ref p2 1)))))
