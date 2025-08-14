
#lang racket
(require sketching)
(provide draw-tunnel)

;; Real modulo for positive modulus
(define (real-modulo x m)
  (let ([r (- x (* m (floor (/ x m))))])
    (if (< r 0) (+ r m) r)))

(define WIDTH 1920)
(define HEIGHT 1080)

(define N-RINGS 18)
(define N-POINTS 32)
(define TUNNEL_LENGTH 2000)
(define RADIUS 400)
(define SPEED 30)



(define (project x y z)
  (let* ([fov 800]
         [scale (/ fov z)])
    (list (+ (/ WIDTH 2) (* x scale))
          (+ (/ HEIGHT 2) (* y scale)))))

;; Accepts a background color argument
(define (draw-tunnel bg)
  (background bg)
  (define t (/ (current-inexact-milliseconds) 1000.0))
  (define dz SPEED)
  (define rings
    (for/list ([i (in-range N-RINGS)])
  (let* ([z (+ 200 (* (/ i N-RINGS) TUNNEL_LENGTH) (real-modulo (* t dz) TUNNEL_LENGTH))]
     [z (real-modulo z TUNNEL_LENGTH)]
     [z (+ 1 z)]) ; avoid div by zero
        (for/list ([j (in-range N-POINTS)])
          (let* ([angle (* 2 pi (/ j N-POINTS))]
                 [x (* RADIUS (cos angle))]
                 [y (* RADIUS (sin angle))])
            (project x y z))))))
  (stroke (color 255 140 0))
  (stroke-weight 6)
  ;; Draw rings
  (for ([ring rings])
    (for ([j (in-range N-POINTS)])
      (define p1 (list-ref ring j))
      (define p2 (list-ref ring (modulo (+ j 1) N-POINTS)))
      (line (list-ref p1 0) (list-ref p1 1) (list-ref p2 0) (list-ref p2 1))))
  ;; Draw lines between rings
  (for ([i (in-range (- N-RINGS 1))])
    (define ring1 (list-ref rings i))
    (define ring2 (list-ref rings (+ i 1)))
    (for ([j (in-range N-POINTS)])
      (define p1 (list-ref ring1 j))
      (define p2 (list-ref ring2 j))
      (line (list-ref p1 0) (list-ref p1 1) (list-ref p2 0) (list-ref p2 1)))))
