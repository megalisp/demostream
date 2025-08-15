#lang racket
(require sketching)

(provide draw-pipes)

(define pipe-count 16)
(define points-per-pipe 80)
(define turn-interval 18)

;; Each pipe: (list points color dir step)
(define pipes (make-vector pipe-count))


(define (clamp v minv maxv)
  (max minv (min maxv v)))

(define (random-orange)
  (let ([base 140])
    (list (clamp (+ 200 (random 56)) 0 255)
          (clamp (+ base (random 80)) 0 255)
          (clamp (random 40) 0 255))))


(define (random-dir)
  ;; 6 axis-aligned directions in 3D
  (list-ref '((1 0 0) (-1 0 0) (0 1 0) (0 -1 0) (0 0 1) (0 0 -1)) (inexact->exact (floor (random 6)))))


(define (reset-pipes!)
  (for ([i (in-range pipe-count)])
    (define color (random-orange))
    (define start (list (- (inexact->exact (floor (random 200))) 100)
                       (- (inexact->exact (floor (random 200))) 100)
                       (- (inexact->exact (floor (random 200))) 100)))
    (define dir (random-dir))
    (define points (make-list points-per-pipe start))
    (vector-set! pipes i (list points color dir 0))))


(define (project3d pt)
  (define x (car pt))
  (define y (cadr pt))
  (define z (caddr pt))
  (define fov 400.0)
  (define viewer-z 600.0)
  (define scale (/ fov (+ viewer-z z 1)))
  (list (+ (/ (width) 2) (* x scale))
        (+ (/ (height) 2) (* y scale))))


(define (step-pipe pipe)
  (define points (car pipe))
  (define color (cadr pipe))
  (define dir (caddr pipe))
  (define step (cadddr pipe))
  (define head (car points))
  (define speed 9) ; increase for even faster movement
  (define new-head (map (lambda (h d) (+ h (* d speed))) head dir))
  (define new-points (cons new-head (take points (- points-per-pipe 1))))
  (define new-step (+ step 1))
  (define new-dir (if (zero? (modulo new-step turn-interval))
                      (let loop ()
                        (define d (random-dir))
                        (if (equal? d (map - dir)) (loop) d))
                      dir))
  (list new-points color new-dir new-step))

(define (draw-pipes bg)
  (background bg)
  (when (or (not (= (vector-length pipes) pipe-count))
            (not (list? (vector-ref pipes 0))))
    (reset-pipes!))
  (for ([i (in-range pipe-count)])
    (define pipe (vector-ref pipes i))
  (define color (cadr pipe))
  (define points (car pipe))
    (for ([j (in-range (- (length points) 1))])
      (define p1 (project3d (list-ref points j)))
      (define p2 (project3d (list-ref points (+ j 1))))
      (stroke-weight 14)
      (stroke (car color) (cadr color) (caddr color) 200)
  (line (car p1) (cadr p1) (car p2) (cadr p2))))
  ;; update pipes
  (for ([i (in-range pipe-count)])
    (vector-set! pipes i (step-pipe (vector-ref pipes i)))))
